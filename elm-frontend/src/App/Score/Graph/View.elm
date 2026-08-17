module App.Score.Graph.View exposing (GraphState(..), view)

import App.Score.Graph.Compact as Compact exposing (VisualEdge, VisualKind(..), VisualNode)
import App.Score.Graph.Layout as Layout exposing (Box, LayoutResult)
import App.Score.Graph.Types as Types exposing (GraphNode, NodeKind(..))
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlAttr
import Html.Styled.Events as Events
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttr


type GraphState
    = NotCompiled
    | Compiling
    | Compiled LayoutResult
    | GraphFailed String


view : { onRedraw : msg } -> GraphState -> Html msg
view { onRedraw } state =
    Html.div
        [ HtmlAttr.css
            [ position relative
            , flex (int 1)
            , minHeight (px 0)
            , displayFlex
            , backgroundColor (hex "1a1d2e")
            , overflow Css.hidden
            ]
        ]
        [ Html.button
            [ HtmlAttr.css redrawButtonStyle
            , Events.onClick onRedraw
            ]
            [ Html.text "redesenhar" ]
        , content state
        ]


redrawButtonStyle : List Style
redrawButtonStyle =
    [ position absolute
    , top (px 4)
    , right (px 4)
    , zIndex (int 10)
    , backgroundColor (hex "263238")
    , border3 (px 1) solid (hex "37474f")
    , color (hex "cdd6f4")
    , fontSize (rem 0.5)
    , padding2 (px 2) (px 6)
    , cursor pointer
    , hover [ backgroundColor (hex "37474f") ]
    ]


content : GraphState -> Html msg
content state =
    case state of
        NotCompiled ->
            emptyMessage "clique em \"redesenhar\" para ver o grafo"

        Compiling ->
            emptyMessage "compilando..."

        GraphFailed message ->
            errorMessage message

        Compiled result ->
            graphSvg result


emptyMessage : String -> Html msg
emptyMessage message =
    Html.div
        [ HtmlAttr.css
            [ padding (rem 0.3)
            , color (hex "6272a4")
            , fontSize (rem 0.5)
            , fontFamilies [ "monospace" ]
            ]
        ]
        [ Html.text message ]


errorMessage : String -> Html msg
errorMessage message =
    Html.div
        [ HtmlAttr.css
            [ padding (rem 0.3)
            , color (hex "f07178")
            , fontSize (rem 0.5)
            , fontFamilies [ "monospace" ]
            , Css.property "white-space" "pre-wrap"
            ]
        ]
        [ Html.text message ]


graphSvg : LayoutResult -> Html msg
graphSvg result =
    let
        boxesList =
            Dict.values result.boxes

        margin =
            24

        minX =
            boxesList |> List.map .x |> List.minimum |> Maybe.withDefault 0

        minY =
            boxesList |> List.map .y |> List.minimum |> Maybe.withDefault 0

        maxX =
            boxesList |> List.map (\b -> b.x + b.width) |> List.maximum |> Maybe.withDefault 0

        maxY =
            boxesList |> List.map (\b -> b.y + b.height) |> List.maximum |> Maybe.withDefault 0

        viewBoxStr =
            String.join " "
                (List.map String.fromFloat
                    [ minX - margin
                    , minY - margin
                    , maxX - minX + margin * 2
                    , maxY - minY + margin * 2
                    ]
                )
    in
    Svg.svg
        [ SvgAttr.viewBox viewBoxStr
        , SvgAttr.css [ flex (int 1), minWidth (px 0) ]
        ]
        (arrowDefs
            :: List.filterMap (edgeView result.boxes) result.edges
            ++ List.filterMap (nodeView result.boxes) result.nodes
        )


arrowDefs : Svg msg
arrowDefs =
    Svg.defs []
        [ Svg.marker
            [ SvgAttr.id "score-graph-arrow"
            , SvgAttr.viewBox "0 0 10 10"
            , SvgAttr.refX "9"
            , SvgAttr.refY "5"
            , SvgAttr.markerWidth "6"
            , SvgAttr.markerHeight "6"
            , SvgAttr.orient "auto-start-reverse"
            ]
            [ Svg.path
                [ SvgAttr.d "M 0 0 L 10 5 L 0 10 z"
                , SvgAttr.fill "#546e7a"
                ]
                []
            ]
        ]


nodeIntersection : Box -> Box -> ( Float, Float )
nodeIntersection intersectionBox targetBox =
    let
        w =
            intersectionBox.width / 2

        h =
            intersectionBox.height / 2

        x2 =
            intersectionBox.x + w

        y2 =
            intersectionBox.y + h

        x1 =
            targetBox.x + targetBox.width / 2

        y1 =
            targetBox.y + targetBox.height / 2

        xx1 =
            (x1 - x2) / (2 * w) - (y1 - y2) / (2 * h)

        yy1 =
            (x1 - x2) / (2 * w) + (y1 - y2) / (2 * h)

        denom =
            abs xx1 + abs yy1

        a =
            1
                / (if denom == 0 then
                    1

                   else
                    denom
                  )

        xx3 =
            a * xx1

        yy3 =
            a * yy1
    in
    ( w * (xx3 + yy3) + x2, h * (-xx3 + yy3) + y2 )


edgeView : Dict Int Box -> VisualEdge -> Maybe (Svg msg)
edgeView boxes edge =
    Maybe.map2
        (\sourceBox targetBox ->
            let
                ( sx, sy ) =
                    nodeIntersection sourceBox targetBox

                ( tx, ty ) =
                    nodeIntersection targetBox sourceBox

                d =
                    "M "
                        ++ String.fromFloat sx
                        ++ ","
                        ++ String.fromFloat sy
                        ++ " L "
                        ++ String.fromFloat tx
                        ++ ","
                        ++ String.fromFloat ty
            in
            Svg.path
                [ SvgAttr.d d
                , SvgAttr.stroke "#546e7a"
                , SvgAttr.strokeWidth "1"
                , SvgAttr.fill "none"
                , SvgAttr.markerEnd "url(#score-graph-arrow)"
                ]
                []
        )
        (Dict.get edge.source boxes)
        (Dict.get edge.target boxes)


nodeView : Dict Int Box -> VisualNode -> Maybe (Svg msg)
nodeView boxes node =
    Dict.get node.id boxes
        |> Maybe.map
            (\box ->
                Svg.foreignObject
                    [ SvgAttr.x (String.fromFloat box.x)
                    , SvgAttr.y (String.fromFloat box.y)
                    , SvgAttr.width (String.fromFloat box.width)
                    , SvgAttr.height (String.fromFloat box.height)
                    ]
                    [ nodeCard node ]
            )


kindColor : GraphNode -> Style
kindColor node =
    case node.kind of
        PassthroughKind ->
            Css.batch [ borderStyle dashed, borderColor (hex "546e7a"), color (hex "546e7a") ]

        TransformPushKind ->
            borderColor (hex "ffcb6b")

        TransformPopKind ->
            borderColor (hex "ffcb6b")

        _ ->
            Css.batch []


cardStyle : VisualNode -> List Style
cardStyle node =
    let
        base =
            [ boxSizing borderBox
            , Css.height (pct 100)
            , padding2 (px 3) (px 3)
            , border3 (px 1) solid (hex "37474f")
            , backgroundColor (hex "263238")
            , color (hex "cdd6f4")
            , textAlign left
            , fontSize (rem 0.5)
            , fontFamilies [ "monospace" ]
            , overflow Css.hidden
            ]

        kindSpecific =
            case node.members of
                first :: _ ->
                    [ kindColor first ]

                [] ->
                    []
    in
    if Layout.isCircle node then
        base
            ++ kindSpecific
            ++ [ borderRadius (pct 50)
               , displayFlex
               , alignItems center
               , justifyContent center
               , textAlign center
               ]
            ++ (case node.visualKind of
                    PlayNode _ ->
                        [ borderColor (hex "c3e88d"), color (hex "c3e88d"), fontWeight bold ]

                    GroupNode ->
                        []
               )

    else
        base ++ kindSpecific


nodeCard : VisualNode -> Html msg
nodeCard node =
    case node.visualKind of
        PlayNode index ->
            Html.div [ HtmlAttr.css (cardStyle node) ]
                [ Html.text ("play " ++ String.fromInt index) ]

        GroupNode ->
            case node.members of
                [] ->
                    Html.text ""

                first :: _ ->
                    Html.div [ HtmlAttr.css (cardStyle node) ]
                        (kindLabel first (List.length node.members) :: bodyFor node first)


kindLabel : GraphNode -> Int -> Html msg
kindLabel node count =
    Html.div
        [ HtmlAttr.css
            [ textTransform uppercase
            , letterSpacing (Css.em 0.05)
            , fontSize (rem 0.4)
            , color (hex "546e7a")
            ]
        ]
        [ Html.text
            (Types.nodeKindLabel node.kind
                ++ (if count > 1 then
                        " x" ++ String.fromInt count

                    else
                        ""
                   )
            )
        ]


bodyFor : VisualNode -> GraphNode -> List (Html msg)
bodyFor node first =
    let
        isSequence =
            List.length node.members > 1
    in
    case first.kind of
        StateKind ->
            if isSequence then
                sequenceSummary node

            else
                noteBody first

        ForkKind ->
            if isSequence then
                sequenceSummary node

            else
                noteBody first

        TransformPushKind ->
            case first.listenChannel of
                Just channel ->
                    [ exprLine ("listen \"" ++ channel ++ "\"") ]

                Nothing ->
                    List.map
                        (\t -> exprLine (t.paramName ++ " = " ++ Types.exprToString t.expr))
                        first.transforms

        JoinKind ->
            case first.joinArity of
                Just arity ->
                    [ arityLine ("arity: " ++ String.fromInt arity) ]

                Nothing ->
                    []

        BranchKind ->
            case first.cond of
                Just cond ->
                    [ exprLine (Types.exprToString cond) ]

                Nothing ->
                    []

        SignalEmitKind ->
            (case first.signalId of
                Just signalId ->
                    [ exprLine ("\"" ++ signalId ++ "\"") ]

                Nothing ->
                    []
            )
                ++ noteBody first

        SkipKind ->
            case first.skipCount of
                Just skipCount ->
                    [ arityLine ("skipped x" ++ String.fromInt skipCount) ]

                Nothing ->
                    []

        RepeatKind ->
            (case first.repeatCount of
                Just repeatCount ->
                    [ arityLine ("n: " ++ String.fromInt repeatCount) ]

                Nothing ->
                    []
            )
                ++ sequenceSummary node

        ReverseKind ->
            sequenceSummary node

        _ ->
            []


noteBody : GraphNode -> List (Html msg)
noteBody node =
    let
        instrument =
            node.params
                |> List.filter (\( name, _ ) -> name == "instrument")
                |> List.head
                |> Maybe.map (\( _, value ) -> Types.paramValueToString value)

        instrumentLine =
            case instrument of
                Just i ->
                    [ Html.div
                        [ HtmlAttr.css [ color (hex "89ddff"), fontWeight bold ] ]
                        [ Html.text i ]
                    ]

                Nothing ->
                    []

        paramLines =
            if List.isEmpty node.params then
                []

            else
                [ Html.div
                    [ HtmlAttr.css
                        [ displayFlex
                        , flexWrap Css.wrap
                        , Css.property "gap" "1px 2px"
                        ]
                    ]
                    (node.params
                        |> List.map
                            (\( name, value ) ->
                                Html.div
                                    [ HtmlAttr.css
                                        [ fontSize (rem 0.4)
                                        , color (hex "c3e88d")
                                        , backgroundColor (rgba 195 232 141 0.1)
                                        ]
                                    ]
                                    [ Html.text (name ++ ": " ++ Types.paramValueToString value) ]
                            )
                    )
                ]
    in
    instrumentLine ++ paramLines


sequenceSummary : VisualNode -> List (Html msg)
sequenceSummary node =
    let
        summary =
            Compact.summarizeSequence node.members

        instrumentLine =
            case summary.instrument of
                Just i ->
                    [ Html.div
                        [ HtmlAttr.css [ color (hex "89ddff"), fontWeight bold ] ]
                        [ Html.text i ]
                    ]

                Nothing ->
                    []

        noteCount =
            node.members |> List.filter (\n -> n.kind == StateKind) |> List.length
    in
    instrumentLine
        ++ [ arityLine
                (String.fromInt noteCount
                    ++ " notas, dur "
                    ++ String.fromFloat summary.totalDur
                )
           ]


exprLine : String -> Html msg
exprLine text =
    Html.div
        [ HtmlAttr.css
            [ fontSize (rem 0.4)
            , color (hex "f78c6c")
            , Css.property "word-break" "break-word"
            ]
        ]
        [ Html.text text ]


arityLine : String -> Html msg
arityLine text =
    Html.div
        [ HtmlAttr.css [ fontSize (rem 0.4), color (hex "546e7a") ] ]
        [ Html.text text ]
