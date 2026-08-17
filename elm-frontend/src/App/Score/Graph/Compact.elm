module App.Score.Graph.Compact exposing
    ( CompactResult
    , VisualEdge
    , VisualKind(..)
    , VisualNode
    , compact
    , longestPathDur
    , summarizeSequence
    )

import App.Score.Graph.Types as Types
    exposing
        ( GraphNode
        , NodeKind(..)
        , ScoreGraph
        )
import Dict exposing (Dict)
import Set exposing (Set)


playNodeIdBase : Int
playNodeIdBase =
    1000000000


type VisualKind
    = GroupNode
    | PlayNode Int


type alias VisualNode =
    { id : Int
    , visualKind : VisualKind
    , members : List GraphNode
    , entryIds : List Int
    , exitIds : List Int
    }


type alias VisualEdge =
    { id : String
    , source : Int
    , target : Int
    }


type alias CompactResult =
    { nodes : List VisualNode
    , edges : List VisualEdge
    }



-- NODE PREDICATES


bodyEntryOf : GraphNode -> Maybe Int
bodyEntryOf node =
    case node.kind of
        ReverseKind ->
            node.reverseBodyEntryId

        RepeatKind ->
            node.repeatBodyEntryId

        _ ->
            Nothing


bodyExitOf : GraphNode -> Maybe Int
bodyExitOf node =
    case node.kind of
        ReverseKind ->
            node.reverseBodyExitId

        RepeatKind ->
            node.repeatBodyExitId

        _ ->
            Nothing


isLoopMarker : GraphNode -> Bool
isLoopMarker node =
    bodyEntryOf node /= Nothing


isAbsorbingMarker : GraphNode -> Bool
isAbsorbingMarker node =
    isLoopMarker node || node.kind == SkipKind


isTransparentLink : GraphNode -> Bool
isTransparentLink node =
    (node.kind == LegatoKind || node.kind == PassthroughKind || node.kind == TransformPopKind)
        && List.length node.next
        == 1


isDeadEndPassthrough : GraphNode -> Bool
isDeadEndPassthrough node =
    node.kind == PassthroughKind && List.isEmpty node.next


resolveVisualTarget : Dict Int GraphNode -> Int -> Maybe Int
resolveVisualTarget nodesById startId =
    resolveVisualTargetHelp nodesById Set.empty startId


resolveVisualTargetHelp : Dict Int GraphNode -> Set Int -> Int -> Maybe Int
resolveVisualTargetHelp nodesById visited id =
    if Set.member id visited then
        Just id

    else
        case Dict.get id nodesById of
            Nothing ->
                Just id

            Just node ->
                if isDeadEndPassthrough node then
                    Nothing

                else if not (isTransparentLink node) then
                    Just id

                else
                    case node.next of
                        next :: _ ->
                            resolveVisualTargetHelp nodesById (Set.insert id visited) next

                        [] ->
                            Just id


resolveAll : Dict Int GraphNode -> List Int -> List Int
resolveAll nodesById ids =
    List.filterMap (resolveVisualTarget nodesById) ids


collectLoopBody : Dict Int GraphNode -> Int -> Int -> List Int
collectLoopBody nodesById bodyEntryId exitId =
    collectLoopBodyHelp nodesById bodyEntryId exitId [ bodyEntryId ] Set.empty []
        |> List.reverse


collectLoopBodyHelp : Dict Int GraphNode -> Int -> Int -> List Int -> Set Int -> List Int -> List Int
collectLoopBodyHelp nodesById bodyEntryId exitId stack visited members =
    case stack of
        [] ->
            members

        id :: rest ->
            if (id == exitId && id /= bodyEntryId) || Set.member id visited then
                collectLoopBodyHelp nodesById bodyEntryId exitId rest visited members

            else
                let
                    visited2 =
                        Set.insert id visited
                in
                case Dict.get id nodesById of
                    Nothing ->
                        collectLoopBodyHelp nodesById bodyEntryId exitId rest visited2 members

                    Just node ->
                        let
                            members2 =
                                id :: members
                        in
                        if id /= bodyEntryId && isLoopMarker node then
                            collectLoopBodyHelp nodesById bodyEntryId exitId rest visited2 members2

                        else
                            collectLoopBodyHelp nodesById bodyEntryId exitId (node.next ++ rest) visited2 members2


type alias SkipRepeatFrame =
    { repeatId : Int
    , remaining : Int
    }


type alias SkipResult =
    { members : List Int
    , continuation : Maybe (List Int)
    }


skipLoopBack : Dict Int GraphNode -> List SkipRepeatFrame -> Int -> Maybe ( List SkipRepeatFrame, List Int )
skipLoopBack nodesById repeatStack nodeId =
    case repeatStack of
        [] ->
            Nothing

        top :: rest ->
            Maybe.andThen
                (\repeatNode ->
                    if bodyExitOf repeatNode /= Just nodeId then
                        Nothing

                    else if top.remaining > 0 then
                        case bodyEntryOf repeatNode of
                            Just entryId ->
                                Just ( { repeatId = top.repeatId, remaining = top.remaining - 1 } :: rest, [ entryId ] )

                            Nothing ->
                                Just ( rest, repeatNode.next )

                    else
                        Just ( rest, repeatNode.next )
                )
                (Dict.get top.repeatId nodesById)


collectSkippedStates : Dict Int GraphNode -> Int -> Int -> SkipResult
collectSkippedStates nodesById startId skipCount =
    collectSkippedStatesHelp nodesById Set.empty [] skipCount startId []


collectSkippedStatesHelp :
    Dict Int GraphNode
    -> Set Int
    -> List SkipRepeatFrame
    -> Int
    -> Int
    -> List Int
    -> SkipResult
collectSkippedStatesHelp nodesById visited repeatStack remaining id membersRev =
    if Set.member id visited then
        { members = List.reverse membersRev, continuation = Nothing }

    else
        case Dict.get id nodesById of
            Nothing ->
                { members = List.reverse membersRev, continuation = Nothing }

            Just node ->
                let
                    visited2 =
                        Set.insert id visited

                    members2 =
                        id :: membersRev
                in
                case ( bodyEntryOf node, node.repeatCount ) of
                    ( Just entryId, Just repeatCount ) ->
                        collectSkippedStatesHelp nodesById
                            visited2
                            ({ repeatId = node.id, remaining = repeatCount - 1 } :: repeatStack)
                            remaining
                            entryId
                            members2

                    _ ->
                        let
                            remaining2 =
                                if node.kind == StateKind then
                                    remaining - 1

                                else
                                    remaining
                        in
                        if node.kind == StateKind && remaining2 <= 0 then
                            case skipLoopBack nodesById repeatStack node.id of
                                Just ( _, continuation ) ->
                                    { members = List.reverse members2, continuation = Just continuation }

                                Nothing ->
                                    { members = List.reverse members2, continuation = Just node.next }

                        else
                            case skipLoopBack nodesById repeatStack node.id of
                                Just ( newStack, looped ) ->
                                    case looped of
                                        [ only ] ->
                                            collectSkippedStatesHelp nodesById visited2 newStack remaining2 only members2

                                        other ->
                                            { members = List.reverse members2, continuation = Just other }

                                Nothing ->
                                    case node.next of
                                        [ only ] ->
                                            collectSkippedStatesHelp nodesById visited2 repeatStack remaining2 only members2

                                        other ->
                                            { members = List.reverse members2, continuation = Just other }


type alias UnionState =
    { nodeGroup : Dict Int Int
    , groups : Dict Int (List GraphNode)
    }


initUnion : List GraphNode -> UnionState
initUnion nodes =
    List.foldl
        (\n acc ->
            { nodeGroup = Dict.insert n.id n.id acc.nodeGroup
            , groups = Dict.insert n.id [ n ] acc.groups
            }
        )
        { nodeGroup = Dict.empty, groups = Dict.empty }
        nodes


union : Int -> Int -> UnionState -> UnionState
union a b state =
    case ( Dict.get a state.nodeGroup, Dict.get b state.nodeGroup ) of
        ( Just rootA, Just rootB ) ->
            if rootA == rootB then
                state

            else
                case ( Dict.get rootA state.groups, Dict.get rootB state.groups ) of
                    ( Just groupA, Just groupB ) ->
                        let
                            nodeGroup2 =
                                List.foldl (\n acc -> Dict.insert n.id rootA acc) state.nodeGroup groupB
                        in
                        { nodeGroup = nodeGroup2
                        , groups =
                            state.groups
                                |> Dict.insert rootA (groupA ++ groupB)
                                |> Dict.remove rootB
                        }

                    _ ->
                        state

        _ ->
            state


resolveAbsorptionRoots : Dict Int (List Int) -> Int -> List Int
resolveAbsorptionRoots absorbedBy id =
    resolveAbsorptionRootsHelp absorbedBy Set.empty id Set.empty
        |> Set.toList


resolveAbsorptionRootsHelp : Dict Int (List Int) -> Set Int -> Int -> Set Int -> Set Int
resolveAbsorptionRootsHelp absorbedBy path current rootsAcc =
    case Dict.get current absorbedBy of
        Nothing ->
            Set.insert current rootsAcc

        Just owners ->
            if Set.member current path then
                Set.insert current rootsAcc

            else
                let
                    path2 =
                        Set.insert current path
                in
                List.foldl (\owner acc -> resolveAbsorptionRootsHelp absorbedBy path2 owner acc) rootsAcc owners


hasRealTransform : GraphNode -> Bool
hasRealTransform node =
    node.kind
        /= StateKind
        && node.kind
        /= JoinKind
        && node.kind
        /= ForkKind
        && not (List.isEmpty node.transforms)


forkContainsRealTransform : Dict Int GraphNode -> Int -> Bool
forkContainsRealTransform nodesById forkId =
    case Dict.get forkId nodesById of
        Nothing ->
            False

        Just fork ->
            forkContainsRealTransformHelp nodesById Set.empty fork.next


forkContainsRealTransformHelp : Dict Int GraphNode -> Set Int -> List Int -> Bool
forkContainsRealTransformHelp nodesById visited stack =
    case stack of
        [] ->
            False

        id :: rest ->
            if Set.member id visited then
                forkContainsRealTransformHelp nodesById visited rest

            else
                let
                    visited2 =
                        Set.insert id visited
                in
                case Dict.get id nodesById of
                    Nothing ->
                        forkContainsRealTransformHelp nodesById visited2 rest

                    Just node ->
                        if node.kind == JoinKind then
                            forkContainsRealTransformHelp nodesById visited2 rest

                        else if hasRealTransform node || isAbsorbingMarker node then
                            True

                        else
                            forkContainsRealTransformHelp nodesById visited2 (node.next ++ rest)


isGroupable : Dict Int GraphNode -> GraphNode -> Bool
isGroupable nodesById node =
    case node.kind of
        StateKind ->
            True

        JoinKind ->
            True

        ForkKind ->
            not (forkContainsRealTransform nodesById node.id)

        _ ->
            False


addOwner : Int -> Int -> Dict Int (List Int) -> Dict Int (List Int)
addOwner memberId ownerId absorbedBy =
    let
        owners =
            Dict.get memberId absorbedBy |> Maybe.withDefault []
    in
    if List.member ownerId owners then
        absorbedBy

    else
        Dict.insert memberId (ownerId :: owners) absorbedBy


compact : ScoreGraph -> CompactResult
compact fullGraph =
    let
        nodesByIdRaw : Dict Int GraphNode
        nodesByIdRaw =
            Dict.fromList (List.map (\n -> ( n.id, n )) fullGraph.nodes)

        absorbedByAfterLoops : Dict Int (List Int)
        absorbedByAfterLoops =
            List.foldl
                (\n acc ->
                    case ( bodyEntryOf n, bodyExitOf n ) of
                        ( Just entryId, Just exitId ) ->
                            List.foldl (\memberId a -> addOwner memberId n.id a)
                                acc
                                (collectLoopBody nodesByIdRaw entryId exitId)

                        _ ->
                            acc
                )
                Dict.empty
                fullGraph.nodes

        ( absorbedBy, skipContinuation ) =
            List.foldl
                (\n ( absAcc, contAcc ) ->
                    case ( n.kind, n.next, n.skipCount ) of
                        ( SkipKind, [ next ], Just skipCount ) ->
                            let
                                result =
                                    collectSkippedStates nodesByIdRaw next skipCount

                                absAcc2 =
                                    List.foldl (\memberId a -> addOwner memberId n.id a) absAcc result.members
                            in
                            case result.continuation of
                                Just continuation ->
                                    ( absAcc2, Dict.insert n.id continuation contAcc )

                                Nothing ->
                                    ( absAcc2, contAcc )

                        _ ->
                            ( absAcc, contAcc )
                )
                ( absorbedByAfterLoops, Dict.empty )
                fullGraph.nodes

        filteredNodes : List GraphNode
        filteredNodes =
            fullGraph.nodes
                |> List.filter
                    (\n ->
                        not (isTransparentLink n)
                            && not (isDeadEndPassthrough n)
                            && not (Dict.member n.id absorbedBy)
                    )
                |> List.map
                    (\n ->
                        { n
                            | next =
                                if n.kind == SkipKind then
                                    case Dict.get n.id skipContinuation of
                                        Just continuation ->
                                            resolveAll nodesByIdRaw continuation

                                        Nothing ->
                                            resolveAll nodesByIdRaw n.next

                                else
                                    resolveAll nodesByIdRaw n.next
                        }
                    )

        nodesById : Dict Int GraphNode
        nodesById =
            Dict.fromList (List.map (\n -> ( n.id, n )) filteredNodes)

        inDegree : Dict Int Int
        inDegree =
            List.foldl
                (\n acc ->
                    List.foldl (\target a -> Dict.update target (\v -> Just (Maybe.withDefault 0 v + 1)) a) acc n.next
                )
                Dict.empty
                filteredNodes

        chainPrev : Dict Int (List Int)
        chainPrev =
            List.foldl
                (\n acc ->
                    if not (isGroupable nodesById n) then
                        acc

                    else
                        List.foldl
                            (\nextId a ->
                                case Dict.get nextId nodesById of
                                    Just target ->
                                        if isGroupable nodesById target then
                                            Dict.update nextId (\v -> Just (n.id :: Maybe.withDefault [] v)) a

                                        else
                                            a

                                    Nothing ->
                                        a
                            )
                            acc
                            n.next
                )
                Dict.empty
                filteredNodes

        chainable : Set Int
        chainable =
            Dict.foldl
                (\targetId preds acc ->
                    if List.length preds == Maybe.withDefault 0 (Dict.get targetId inDegree) then
                        Set.insert targetId acc

                    else
                        acc
                )
                Set.empty
                chainPrev

        unionAfterChains : UnionState
        unionAfterChains =
            Set.foldl
                (\targetId acc ->
                    List.foldl (\predId a -> union predId targetId a) acc (Dict.get targetId chainPrev |> Maybe.withDefault [])
                )
                (initUnion filteredNodes)
                chainable

        unionAfterJoins : UnionState
        unionAfterJoins =
            List.foldl
                (\n acc ->
                    case n.next of
                        [ onlyTarget ] ->
                            if n.kind == JoinKind then
                                case Dict.get onlyTarget nodesById of
                                    Just target ->
                                        if target.kind == StateKind && Maybe.withDefault 0 (Dict.get target.id inDegree) == 1 then
                                            union n.id target.id acc

                                        else
                                            acc

                                    Nothing ->
                                        acc

                            else
                                acc

                        _ ->
                            acc
                )
                unionAfterChains
                filteredNodes

        finalUnion : UnionState
        finalUnion =
            Dict.foldl
                (\memberId _ acc ->
                    case Dict.get memberId nodesByIdRaw of
                        Nothing ->
                            acc

                        Just member ->
                            if isTransparentLink member || isDeadEndPassthrough member then
                                acc

                            else
                                let
                                    rootIds =
                                        resolveAbsorptionRoots absorbedBy memberId
                                in
                                case rootIds of
                                    [] ->
                                        acc

                                    firstRoot :: _ ->
                                        { nodeGroup = Dict.insert memberId firstRoot acc.nodeGroup
                                        , groups =
                                            List.foldl
                                                (\rootId groups ->
                                                    Dict.update rootId
                                                        (\v -> Just (member :: Maybe.withDefault [] v))
                                                        groups
                                                )
                                                acc.groups
                                                rootIds
                                        }
                )
                unionAfterJoins
                absorbedBy

        representative : Dict Int Int
        representative =
            Dict.foldl
                (\root group acc ->
                    List.foldl (\n a -> Dict.insert n.id root a) acc group
                )
                Dict.empty
                finalUnion.groups

        predecessors : Dict Int (List Int)
        predecessors =
            List.foldl
                (\n acc ->
                    List.foldl (\target a -> Dict.update target (\v -> Just (n.id :: Maybe.withDefault [] v)) a) acc n.next
                )
                Dict.empty
                filteredNodes

        isPlayable : GraphNode -> Bool
        isPlayable n =
            n.kind == StateKind || n.kind == ForkKind

        groupNodes : List VisualNode
        groupNodes =
            finalUnion.groups
                |> Dict.toList
                |> List.map
                    (\( startId, group ) ->
                        let
                            marker =
                                group |> List.filter (\n -> n.id == startId && isAbsorbingMarker n) |> List.head
                        in
                        case marker of
                            Just m ->
                                { id = startId
                                , visualKind = GroupNode
                                , members = m :: List.filter (\n -> n.id /= m.id) group
                                , entryIds = []
                                , exitIds = []
                                }

                            Nothing ->
                                let
                                    groupIds =
                                        Set.fromList (List.map .id group)

                                    playable =
                                        List.filter isPlayable group

                                    entryIdsRaw =
                                        playable
                                            |> List.filter
                                                (\n ->
                                                    not (List.any (\p -> Set.member p groupIds) (Dict.get n.id predecessors |> Maybe.withDefault []))
                                                )
                                            |> List.map .id

                                    exitIdsRaw =
                                        playable
                                            |> List.filter (\n -> not (List.any (\nextId -> Set.member nextId groupIds) n.next))
                                            |> List.map .id

                                    entryIds =
                                        if List.isEmpty entryIdsRaw && not (List.isEmpty playable) then
                                            List.take 1 (List.map .id playable)

                                        else
                                            entryIdsRaw

                                    exitIds =
                                        if List.isEmpty exitIdsRaw && not (List.isEmpty playable) then
                                            playable |> List.map .id |> List.reverse |> List.take 1

                                        else
                                            exitIdsRaw
                                in
                                { id = startId
                                , visualKind = GroupNode
                                , members = group
                                , entryIds = entryIds
                                , exitIds = exitIds
                                }
                    )

        playNodes : List VisualNode
        playNodes =
            fullGraph.entries
                |> List.indexedMap
                    (\index _ ->
                        { id = playNodeIdBase + index
                        , visualKind = PlayNode index
                        , members = []
                        , entryIds = []
                        , exitIds = []
                        }
                    )

        internalEdges : List VisualEdge
        internalEdges =
            List.foldl
                (\n ( seen, acc ) ->
                    case Dict.get n.id representative of
                        Nothing ->
                            ( seen, acc )

                        Just sourceRep ->
                            List.foldl
                                (\target ( seen2, acc2 ) ->
                                    case Dict.get target representative of
                                        Nothing ->
                                            ( seen2, acc2 )

                                        Just targetRep ->
                                            if targetRep == sourceRep then
                                                ( seen2, acc2 )

                                            else
                                                let
                                                    edgeId =
                                                        String.fromInt sourceRep ++ "->" ++ String.fromInt targetRep
                                                in
                                                if Set.member edgeId seen2 then
                                                    ( seen2, acc2 )

                                                else
                                                    ( Set.insert edgeId seen2
                                                    , { id = edgeId, source = sourceRep, target = targetRep } :: acc2
                                                    )
                                )
                                ( seen, acc )
                                n.next
                )
                ( Set.empty, [] )
                filteredNodes
                |> Tuple.second

        playEdges : List VisualEdge
        playEdges =
            fullGraph.entries
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, machine ) ( seen, acc ) ->
                        let
                            playId =
                                playNodeIdBase + index
                        in
                        List.foldl
                            (\entryId ( seen2, acc2 ) ->
                                case resolveVisualTarget nodesByIdRaw entryId of
                                    Nothing ->
                                        ( seen2, acc2 )

                                    Just targetId ->
                                        case Dict.get targetId representative of
                                            Nothing ->
                                                ( seen2, acc2 )

                                            Just targetRep ->
                                                let
                                                    edgeId =
                                                        String.fromInt playId ++ "->" ++ String.fromInt targetRep
                                                in
                                                if Set.member edgeId seen2 then
                                                    ( seen2, acc2 )

                                                else
                                                    ( Set.insert edgeId seen2
                                                    , { id = edgeId, source = playId, target = targetRep } :: acc2
                                                    )
                            )
                            ( seen, acc )
                            machine
                    )
                    ( Set.empty, [] )
                |> Tuple.second
    in
    { nodes = groupNodes ++ playNodes
    , edges = internalEdges ++ playEdges
    }


longestPathDur : List GraphNode -> Float
longestPathDur nodes =
    let
        inGroup =
            Set.fromList (List.map .id nodes)

        byId =
            Dict.fromList (List.map (\n -> ( n.id, n )) nodes)

        ownDur : GraphNode -> Float
        ownDur node =
            node.params
                |> List.filter (\( name, _ ) -> name == "dur")
                |> List.head
                |> Maybe.andThen (\( _, value ) -> Types.paramValueToFloat value)
                |> Maybe.withDefault 0

        visit : Set Int -> Dict Int Float -> Int -> ( Dict Int Float, Float )
        visit inProgress memo id =
            case Dict.get id memo of
                Just cached ->
                    ( memo, cached )

                Nothing ->
                    if Set.member id inProgress then
                        ( memo, 0 )

                    else
                        case Dict.get id byId of
                            Nothing ->
                                ( memo, 0 )

                            Just node ->
                                let
                                    inProgress2 =
                                        Set.insert id inProgress

                                    ( memo2, best ) =
                                        List.foldl
                                            (\nextId ( m, b ) ->
                                                if Set.member nextId inGroup then
                                                    let
                                                        ( m2, childDur ) =
                                                            visit inProgress2 m nextId
                                                    in
                                                    ( m2, max b childDur )

                                                else
                                                    ( m, b )
                                            )
                                            ( memo, 0 )
                                            node.next

                                    result =
                                        ownDur node + best
                                in
                                ( Dict.insert id result memo2, result )

        roots =
            let
                hasIncoming id =
                    List.any (\other -> Set.member other.id inGroup && List.member id other.next) nodes
            in
            case List.filter (\n -> not (hasIncoming n.id)) nodes of
                [] ->
                    nodes

                rs ->
                    rs
    in
    List.foldl
        (\n ( memo, best ) ->
            let
                ( memo2, dur ) =
                    visit Set.empty memo n.id
            in
            ( memo2, max best dur )
        )
        ( Dict.empty, 0 )
        roots
        |> Tuple.second


summarizeSequence : List GraphNode -> { instrument : Maybe String, totalDur : Float }
summarizeSequence nodes =
    let
        instruments =
            nodes
                |> List.filterMap
                    (\n ->
                        n.params
                            |> List.filter (\( name, _ ) -> name == "instrument")
                            |> List.head
                            |> Maybe.map (\( _, value ) -> Types.paramValueToString value)
                    )
                |> List.foldl
                    (\i acc ->
                        if List.member i acc then
                            acc

                        else
                            i :: acc
                    )
                    []

        instrument =
            case instruments of
                [ only ] ->
                    Just only

                [] ->
                    Nothing

                _ ->
                    Just "many"
    in
    { instrument = instrument, totalDur = longestPathDur nodes }
