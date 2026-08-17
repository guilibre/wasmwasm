module App.Score.Graph.Types exposing
    ( BinOp(..)
    , ExprNode(..)
    , GraphNode
    , NodeKind(..)
    , ParamValue(..)
    , ScoreGraph
    , ScoreScale
    , TransformEntry
    , decodeScoreGraph
    , exprToString
    , nodeKindLabel
    , paramValueToFloat
    , paramValueToString
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField name decoder =
    Decode.maybe (Decode.field name decoder)


optionalListField : String -> Decoder a -> Decoder (List a)
optionalListField name decoder =
    Decode.oneOf
        [ Decode.field name (Decode.list decoder)
        , Decode.succeed []
        ]


type NodeKind
    = StateKind
    | ForkKind
    | JoinKind
    | PassthroughKind
    | TransformPushKind
    | TransformPopKind
    | BranchKind
    | SignalEmitKind
    | ReverseKind
    | LegatoKind
    | SkipKind
    | RepeatKind


nodeKindLabel : NodeKind -> String
nodeKindLabel kind =
    case kind of
        StateKind ->
            "state"

        ForkKind ->
            "fork"

        JoinKind ->
            "join"

        PassthroughKind ->
            "passthrough"

        TransformPushKind ->
            "push"

        TransformPopKind ->
            "pop"

        BranchKind ->
            "choose"

        SignalEmitKind ->
            "emit"

        ReverseKind ->
            "reverse"

        LegatoKind ->
            "legato"

        SkipKind ->
            "skip"

        RepeatKind ->
            "repeat"


decodeNodeKind : Decoder NodeKind
decodeNodeKind =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "state" ->
                        Decode.succeed StateKind

                    "fork" ->
                        Decode.succeed ForkKind

                    "join" ->
                        Decode.succeed JoinKind

                    "passthrough" ->
                        Decode.succeed PassthroughKind

                    "transform_push" ->
                        Decode.succeed TransformPushKind

                    "transform_pop" ->
                        Decode.succeed TransformPopKind

                    "branch" ->
                        Decode.succeed BranchKind

                    "signal_emit" ->
                        Decode.succeed SignalEmitKind

                    "reverse" ->
                        Decode.succeed ReverseKind

                    "legato" ->
                        Decode.succeed LegatoKind

                    "skip" ->
                        Decode.succeed SkipKind

                    "repeat" ->
                        Decode.succeed RepeatKind

                    other ->
                        Decode.fail ("unknown node kind: " ++ other)
            )


type BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
    | And
    | Or


binOpSymbol : BinOp -> String
binOpSymbol op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        Pow ->
            "^"

        Eq ->
            "=="

        Neq ->
            "!="

        Lt ->
            "<"

        Gt ->
            ">"

        Lte ->
            "<="

        Gte ->
            ">="

        And ->
            "&"

        Or ->
            "|"


decodeBinOp : Decoder BinOp
decodeBinOp =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "add" ->
                        Decode.succeed Add

                    "sub" ->
                        Decode.succeed Sub

                    "mul" ->
                        Decode.succeed Mul

                    "div" ->
                        Decode.succeed Div

                    "mod" ->
                        Decode.succeed Mod

                    "pow" ->
                        Decode.succeed Pow

                    "eq" ->
                        Decode.succeed Eq

                    "neq" ->
                        Decode.succeed Neq

                    "lt" ->
                        Decode.succeed Lt

                    "gt" ->
                        Decode.succeed Gt

                    "lte" ->
                        Decode.succeed Lte

                    "gte" ->
                        Decode.succeed Gte

                    "and" ->
                        Decode.succeed And

                    "or" ->
                        Decode.succeed Or

                    other ->
                        Decode.fail ("unknown binop: " ++ other)
            )


decodeWireNumber : Decoder Float
decodeWireNumber =
    Decode.oneOf
        [ Decode.float
        , Decode.map2 (\num den -> num / den)
            (Decode.field "num" Decode.float)
            (Decode.field "den" Decode.float)
        ]


type ParamValue
    = NumberParam Float
    | StringParam String


decodeParamValue : Decoder ParamValue
decodeParamValue =
    Decode.oneOf
        [ Decode.map NumberParam decodeWireNumber
        , Decode.map StringParam Decode.string
        ]


paramValueToFloat : ParamValue -> Maybe Float
paramValueToFloat value =
    case value of
        NumberParam n ->
            Just n

        StringParam _ ->
            Nothing


formatNumber : Float -> String
formatNumber n =
    if n == toFloat (round n) then
        String.fromInt (round n)

    else
        String.fromFloat n


paramValueToString : ParamValue -> String
paramValueToString value =
    case value of
        NumberParam n ->
            formatNumber n

        StringParam s ->
            s


type ExprNode
    = NumberExpr Float
    | StringExpr String
    | NullExpr
    | SkipExpr
    | IdentExpr String
    | TernaryExpr ExprNode ExprNode ExprNode
    | BinaryExpr BinOp ExprNode ExprNode


decodeExprNode : Decoder ExprNode
decodeExprNode =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "number" ->
                        Decode.map NumberExpr (Decode.field "value" decodeWireNumber)

                    "string" ->
                        Decode.map StringExpr (Decode.field "value" Decode.string)

                    "null" ->
                        Decode.succeed NullExpr

                    "skip" ->
                        Decode.succeed SkipExpr

                    "ident" ->
                        Decode.map IdentExpr (Decode.field "name" Decode.string)

                    "ternary" ->
                        Decode.succeed TernaryExpr
                            |> andMap (Decode.field "cond" (Decode.lazy (\_ -> decodeExprNode)))
                            |> andMap (Decode.field "then" (Decode.lazy (\_ -> decodeExprNode)))
                            |> andMap (Decode.field "else" (Decode.lazy (\_ -> decodeExprNode)))

                    "binary" ->
                        Decode.succeed BinaryExpr
                            |> andMap (Decode.field "op" decodeBinOp)
                            |> andMap (Decode.field "lhs" (Decode.lazy (\_ -> decodeExprNode)))
                            |> andMap (Decode.field "rhs" (Decode.lazy (\_ -> decodeExprNode)))

                    other ->
                        Decode.fail ("unknown expr kind: " ++ other)
            )


exprToString : ExprNode -> String
exprToString expr =
    case expr of
        NumberExpr n ->
            formatNumber n

        StringExpr s ->
            "\"" ++ s ++ "\""

        NullExpr ->
            "null"

        SkipExpr ->
            "skip"

        IdentExpr name ->
            name

        TernaryExpr cond then_ else_ ->
            exprToString cond ++ " ? " ++ exprToString then_ ++ " : " ++ exprToString else_

        BinaryExpr op lhs rhs ->
            exprToString lhs ++ " " ++ binOpSymbol op ++ " " ++ exprToString rhs


type alias TransformEntry =
    { paramName : String
    , expr : ExprNode
    }


decodeTransformEntry : Decoder TransformEntry
decodeTransformEntry =
    Decode.map2 TransformEntry
        (Decode.field "paramName" Decode.string)
        (Decode.field "expr" decodeExprNode)


type alias GraphNode =
    { id : Int
    , kind : NodeKind
    , params : List ( String, ParamValue )
    , joinArity : Maybe Int
    , transforms : List TransformEntry
    , listenChannel : Maybe String
    , cond : Maybe ExprNode
    , signalId : Maybe String
    , reverseBodyEntryId : Maybe Int
    , reverseBodyExitId : Maybe Int
    , legatoId : Maybe Int
    , skipCount : Maybe Int
    , repeatBodyEntryId : Maybe Int
    , repeatBodyExitId : Maybe Int
    , repeatCount : Maybe Int
    , next : List Int
    }


decodeGraphNode : Decoder GraphNode
decodeGraphNode =
    Decode.succeed GraphNode
        |> andMap (Decode.field "id" Decode.int)
        |> andMap (Decode.field "kind" decodeNodeKind)
        |> andMap
            (Decode.oneOf
                [ Decode.field "params" (Decode.keyValuePairs decodeParamValue)
                , Decode.succeed []
                ]
            )
        |> andMap (optionalField "joinArity" Decode.int)
        |> andMap (optionalListField "transforms" decodeTransformEntry)
        |> andMap (optionalField "listenChannel" Decode.string)
        |> andMap (optionalField "cond" decodeExprNode)
        |> andMap (optionalField "signalId" Decode.string)
        |> andMap (optionalField "reverseBodyEntryId" Decode.int)
        |> andMap (optionalField "reverseBodyExitId" Decode.int)
        |> andMap (optionalField "legatoId" Decode.int)
        |> andMap (optionalField "skipCount" Decode.int)
        |> andMap (optionalField "repeatBodyEntryId" Decode.int)
        |> andMap (optionalField "repeatBodyExitId" Decode.int)
        |> andMap (optionalField "repeatCount" Decode.int)
        |> andMap (optionalListField "next" Decode.int)


type alias ScoreScale =
    { name : String
    , values : List Float
    }


decodeScoreScale : Decoder ScoreScale
decodeScoreScale =
    Decode.map2 ScoreScale
        (Decode.field "name" Decode.string)
        (Decode.field "values" (Decode.list Decode.float))


type alias ScoreGraph =
    { version : Int
    , scales : List ScoreScale
    , nodes : List GraphNode
    , entries : List (List Int)
    , transformPopOfPush : Dict Int Int
    }


decodeScoreGraph : Decoder ScoreGraph
decodeScoreGraph =
    Decode.succeed ScoreGraph
        |> andMap (Decode.field "version" Decode.int)
        |> andMap (Decode.field "scales" (Decode.list decodeScoreScale))
        |> andMap (Decode.field "nodes" (Decode.list decodeGraphNode))
        |> andMap (Decode.field "entries" (Decode.list (Decode.list Decode.int)))
        |> andMap (Decode.field "transformPopOfPush" decodeIntKeyedDict)


decodeIntKeyedDict : Decoder (Dict Int Int)
decodeIntKeyedDict =
    Decode.dict Decode.int
        |> Decode.map
            (Dict.foldl
                (\k v acc ->
                    case String.toInt k of
                        Just key ->
                            Dict.insert key v acc

                        Nothing ->
                            acc
                )
                Dict.empty
            )
