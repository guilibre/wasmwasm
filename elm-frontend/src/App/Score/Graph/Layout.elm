module App.Score.Graph.Layout exposing
    ( Box
    , LayoutResult
    , isCircle
    , layout
    )

import App.Score.Graph.Compact exposing (CompactResult, VisualEdge, VisualKind(..), VisualNode)
import App.Score.Graph.Types exposing (NodeKind(..))
import Dict exposing (Dict)
import Set exposing (Set)


type alias Box =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias LayoutResult =
    { boxes : Dict Int Box
    , nodes : List VisualNode
    , edges : List VisualEdge
    }


rectWidth : Float
rectWidth =
    150


circleSize : Float
circleSize =
    56


playCircleSize : Float
playCircleSize =
    64


lineHeight : Float
lineHeight =
    11


baseHeight : Float
baseHeight =
    22


columnGap : Float
columnGap =
    28


rowGap : Float
rowGap =
    48


isCircle : VisualNode -> Bool
isCircle node =
    case node.visualKind of
        PlayNode _ ->
            True

        GroupNode ->
            case node.members of
                first :: _ ->
                    first.kind == ForkKind || first.kind == JoinKind

                [] ->
                    False


contentLines : VisualNode -> Int
contentLines node =
    case node.visualKind of
        PlayNode _ ->
            0

        GroupNode ->
            case node.members of
                [] ->
                    0

                first :: _ ->
                    let
                        isSequence =
                            List.length node.members > 1

                        noteBodyLines member =
                            let
                                hasInstrument =
                                    List.any (\( name, _ ) -> name == "instrument") member.params
                            in
                            (if hasInstrument then
                                1

                             else
                                0
                            )
                                + ceiling (toFloat (List.length member.params) / 3)
                    in
                    case first.kind of
                        StateKind ->
                            if isSequence then
                                2

                            else
                                noteBodyLines first

                        ForkKind ->
                            if isSequence then
                                2

                            else
                                noteBodyLines first

                        TransformPushKind ->
                            case first.listenChannel of
                                Just _ ->
                                    1

                                Nothing ->
                                    max 1 (List.length first.transforms)

                        JoinKind ->
                            1

                        BranchKind ->
                            1

                        SignalEmitKind ->
                            1 + noteBodyLines first

                        SkipKind ->
                            1

                        RepeatKind ->
                            2

                        ReverseKind ->
                            2

                        _ ->
                            0


estimateBox : VisualNode -> { width : Float, height : Float }
estimateBox node =
    if isCircle node then
        case node.visualKind of
            PlayNode _ ->
                { width = playCircleSize, height = playCircleSize }

            GroupNode ->
                { width = circleSize, height = circleSize }

    else
        { width = rectWidth
        , height = baseHeight + toFloat (contentLines node) * lineHeight
        }


computeLayers : List VisualNode -> List VisualEdge -> Dict Int Int
computeLayers nodes edges =
    let
        successors : Dict Int (List Int)
        successors =
            List.foldl
                (\e acc -> Dict.update e.source (\v -> Just (e.target :: Maybe.withDefault [] v)) acc)
                Dict.empty
                edges

        hasIncoming : Set Int
        hasIncoming =
            Set.fromList (List.map .target edges)

        roots : List Int
        roots =
            case List.filter (\n -> not (Set.member n.id hasIncoming)) nodes of
                [] ->
                    List.map .id nodes |> List.take 1

                rs ->
                    List.map .id rs

        bfs : List Int -> Dict Int Int -> Dict Int Int
        bfs queue layers =
            case queue of
                [] ->
                    layers

                id :: rest ->
                    case Dict.get id layers of
                        Nothing ->
                            bfs rest layers

                        Just currentLayer ->
                            let
                                nexts =
                                    Dict.get id successors |> Maybe.withDefault []

                                ( layers2, toVisit ) =
                                    List.foldl
                                        (\next ( l, tv ) ->
                                            if Dict.member next l then
                                                ( l, tv )

                                            else
                                                ( Dict.insert next (currentLayer + 1) l, next :: tv )
                                        )
                                        ( layers, [] )
                                        nexts
                            in
                            bfs (rest ++ toVisit) layers2

        initialLayers : Dict Int Int
        initialLayers =
            List.foldl (\id acc -> Dict.insert id 0 acc) Dict.empty roots

        afterBfs : Dict Int Int
        afterBfs =
            bfs roots initialLayers
    in
    List.foldl
        (\n acc ->
            if Dict.member n.id acc then
                acc

            else
                Dict.insert n.id 0 acc
        )
        afterBfs
        nodes


groupByLayer : Dict Int Int -> List VisualNode -> List (List Int)
groupByLayer layers nodes =
    let
        maxLayer =
            layers |> Dict.values |> List.maximum |> Maybe.withDefault 0

        idsAtLayer : Int -> List Int
        idsAtLayer target =
            nodes
                |> List.filter (\n -> Dict.get n.id layers == Just target)
                |> List.map .id
    in
    List.range 0 maxLayer |> List.map idsAtLayer


barycenterOrder : Dict Int (List Int) -> List Int -> Dict Int Int -> List Int
barycenterOrder neighborLookup layerIds prevIndex =
    layerIds
        |> List.map
            (\id ->
                let
                    neighborPositions =
                        Dict.get id neighborLookup
                            |> Maybe.withDefault []
                            |> List.filterMap (\n -> Dict.get n prevIndex)
                in
                case neighborPositions of
                    [] ->
                        ( id, toFloat (Maybe.withDefault 0 (Dict.get id prevIndex)) )

                    ps ->
                        ( id, toFloat (List.sum ps) / toFloat (List.length ps) )
            )
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


indexOf : List Int -> Dict Int Int
indexOf ids =
    ids |> List.indexedMap (\i id -> ( id, i )) |> Dict.fromList


orderLayers : List (List Int) -> List VisualEdge -> List (List Int)
orderLayers rawLayers edges =
    let
        predecessors : Dict Int (List Int)
        predecessors =
            List.foldl
                (\e acc -> Dict.update e.target (\v -> Just (e.source :: Maybe.withDefault [] v)) acc)
                Dict.empty
                edges

        successors : Dict Int (List Int)
        successors =
            List.foldl
                (\e acc -> Dict.update e.source (\v -> Just (e.target :: Maybe.withDefault [] v)) acc)
                Dict.empty
                edges

        forwardPass : List (List Int) -> List (List Int)
        forwardPass layersList =
            List.foldl
                (\layerIds acc ->
                    case acc of
                        [] ->
                            [ layerIds ]

                        prev :: _ ->
                            let
                                prevIdx =
                                    indexOf prev

                                ordered =
                                    barycenterOrder predecessors layerIds prevIdx
                            in
                            ordered :: acc
                )
                []
                layersList
                |> List.reverse

        backwardPass : List (List Int) -> List (List Int)
        backwardPass layersList =
            List.foldr
                (\layerIds acc ->
                    case acc of
                        [] ->
                            [ layerIds ]

                        next :: _ ->
                            let
                                nextIdx =
                                    indexOf next

                                ordered =
                                    barycenterOrder successors layerIds nextIdx
                            in
                            ordered :: acc
                )
                []
                layersList
    in
    rawLayers |> forwardPass |> backwardPass |> forwardPass


layout : CompactResult -> LayoutResult
layout compactResult =
    let
        nodes =
            compactResult.nodes

        edges =
            compactResult.edges

        layers =
            computeLayers nodes edges

        orderedLayers =
            orderLayers (groupByLayer layers nodes) edges

        nodesById : Dict Int VisualNode
        nodesById =
            Dict.fromList (List.map (\n -> ( n.id, n )) nodes)

        sizeOf : Int -> { width : Float, height : Float }
        sizeOf id =
            Dict.get id nodesById
                |> Maybe.map estimateBox
                |> Maybe.withDefault { width = rectWidth, height = baseHeight }

        placeLayer : Float -> List Int -> ( Float, Dict Int Box ) -> ( Float, Dict Int Box )
        placeLayer y layerIds ( _, boxesAcc ) =
            let
                sizes =
                    List.map sizeOf layerIds

                totalWidth =
                    List.foldl (\s acc -> acc + s.width + columnGap) -columnGap sizes

                rowHeight =
                    sizes |> List.map .height |> List.maximum |> Maybe.withDefault baseHeight

                place : List Int -> List { width : Float, height : Float } -> Float -> Dict Int Box -> Dict Int Box
                place ids sizesLeft x acc =
                    case ( ids, sizesLeft ) of
                        ( id :: restIds, size :: restSizes ) ->
                            place restIds
                                restSizes
                                (x + size.width + columnGap)
                                (Dict.insert id
                                    { x = x - totalWidth / 2
                                    , y = y
                                    , width = size.width
                                    , height = size.height
                                    }
                                    acc
                                )

                        _ ->
                            acc

                boxes2 =
                    place layerIds sizes 0 boxesAcc
            in
            ( rowHeight, boxes2 )

        ( _, boxes ) =
            List.foldl
                (\layerIds ( yAcc, boxesAcc ) ->
                    let
                        ( rowHeight, boxes2 ) =
                            placeLayer yAcc layerIds ( 0, boxesAcc )
                    in
                    ( yAcc + rowHeight + rowGap, boxes2 )
                )
                ( 0, Dict.empty )
                orderedLayers
    in
    { boxes = boxes, nodes = nodes, edges = edges }
