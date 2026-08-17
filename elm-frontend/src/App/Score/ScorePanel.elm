module App.Score.ScorePanel exposing (..)

import App.Score.Graph.Compact as Compact
import App.Score.Graph.Layout as Layout
import App.Score.Graph.Types as GraphTypes
import App.Score.Graph.View as GraphView
import Browser.Events
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Ports


editorId : String
editorId =
    "score-editor"


minWidthPx : Float
minWidthPx =
    220


maxWidthPx : Float
maxWidthPx =
    1000


defaultWidth : Float
defaultWidth =
    600


minGraphHeightPx : Float
minGraphHeightPx =
    80


maxGraphHeightPx : Float
maxGraphHeightPx =
    1800


defaultGraphHeight : Float
defaultGraphHeight =
    200


type Tab
    = ScoreTab
    | ConductorTab


type DragKind
    = WidthDrag
    | GraphDrag


type alias DragState =
    { kind : DragKind
    , startPos : Float
    , startSize : Float
    }


type SetupState
    = NeedsSetup
    | SetupSent


type alias Model =
    { width : Float
    , isCollapsed : Bool
    , tab : Tab
    , graphHeight : Float
    , drag : Maybe DragState
    , editorSetup : SetupState
    , source : String
    , bpm : String
    , graphState : GraphView.GraphState
    }


init : ( Model, Cmd Msg )
init =
    maybeSendSetup
        { width = defaultWidth
        , isCollapsed = False
        , tab = ScoreTab
        , graphHeight = defaultGraphHeight
        , drag = Nothing
        , editorSetup = NeedsSetup
        , source = ""
        , bpm = "120"
        , graphState = GraphView.NotCompiled
        }
        Cmd.none


type Msg
    = NoOp
    | ToggleCollapse
    | TabChange Tab
    | StartWidthResize Float
    | StartGraphResize Float
    | MouseMove Float Float
    | MouseUp
    | EditorChange String
    | BpmChange String
    | RequestGraphRedraw
    | GraphCompiled Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        next =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                ToggleCollapse ->
                    ( { model | isCollapsed = not model.isCollapsed, editorSetup = NeedsSetup }, Cmd.none )

                TabChange tab ->
                    ( { model | tab = tab, editorSetup = NeedsSetup }, Cmd.none )

                StartWidthResize x ->
                    ( { model
                        | drag = Just { kind = WidthDrag, startPos = x, startSize = model.width }
                      }
                    , Cmd.none
                    )

                StartGraphResize y ->
                    ( { model
                        | drag = Just { kind = GraphDrag, startPos = y, startSize = model.graphHeight }
                      }
                    , Cmd.none
                    )

                MouseMove x y ->
                    case model.drag of
                        Just { kind, startPos, startSize } ->
                            case kind of
                                WidthDrag ->
                                    let
                                        delta =
                                            x - startPos

                                        width =
                                            clamp minWidthPx maxWidthPx (startSize + delta)
                                    in
                                    ( { model | width = width }, Cmd.none )

                                GraphDrag ->
                                    let
                                        delta =
                                            startPos - y

                                        graphHeight =
                                            clamp minGraphHeightPx maxGraphHeightPx (startSize + delta)
                                    in
                                    ( { model | graphHeight = graphHeight }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                MouseUp ->
                    ( { model | drag = Nothing }, Cmd.none )

                EditorChange source ->
                    ( { model | source = source }, Cmd.none )

                BpmChange bpm ->
                    ( { model | bpm = bpm }, Cmd.none )

                RequestGraphRedraw ->
                    ( { model | graphState = GraphView.Compiling }
                    , Ports.scoreGraphUpdate
                        { source = model.source
                        , bpm = bpmFloat model.bpm
                        }
                    )

                GraphCompiled value ->
                    ( { model | graphState = decodeGraphResult value }, Cmd.none )
    in
    maybeSendSetup (Tuple.first next) (Tuple.second next)


type alias ScoreGraphResultPayload =
    { error : Maybe String
    , graph : Maybe GraphTypes.ScoreGraph
    }


decodeScoreGraphResultPayload : Decode.Decoder ScoreGraphResultPayload
decodeScoreGraphResultPayload =
    Decode.map2 ScoreGraphResultPayload
        (Decode.maybe (Decode.field "error" Decode.string))
        (Decode.maybe (Decode.field "graph" GraphTypes.decodeScoreGraph))


decodeGraphResult : Decode.Value -> GraphView.GraphState
decodeGraphResult value =
    case Decode.decodeValue decodeScoreGraphResultPayload value of
        Ok payload ->
            case payload.graph of
                Just graph ->
                    GraphView.Compiled (Layout.layout (Compact.compact graph))

                Nothing ->
                    GraphView.GraphFailed (Maybe.withDefault "erro desconhecido na compilação do score" payload.error)

        Err decodeErr ->
            GraphView.GraphFailed (Decode.errorToString decodeErr)


bpmFloat : String -> Float
bpmFloat raw =
    String.toFloat raw |> Maybe.withDefault 120


bpmValue : Model -> Float
bpmValue model =
    bpmFloat model.bpm


maybeSendSetup : Model -> Cmd Msg -> ( Model, Cmd Msg )
maybeSendSetup model cmd =
    if not model.isCollapsed && model.tab == ScoreTab && model.editorSetup == NeedsSetup then
        ( { model | editorSetup = SetupSent }
        , Cmd.batch
            [ cmd
            , Ports.scorePanelSetup
                { editorId = editorId
                , source = model.source
                , bpm = bpmFloat model.bpm
                }
            ]
        )

    else
        ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove mousePosDecoder
                    , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                    ]

            Nothing ->
                Sub.none
        , Ports.scoreGraphResult GraphCompiled
        ]


mousePosDecoder : Decode.Decoder Msg
mousePosDecoder =
    Decode.map2 MouseMove
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


panelStyle : Float -> List Style
panelStyle width =
    [ flexShrink (int 0)
    , displayFlex
    , flexDirection column
    , Css.property "gap" "6px"
    , padding4 (px 8) (px 12) (px 8) (px 8)
    , backgroundColor (hex "13151f")
    , borderRight3 (px 1) solid (hex "2a2d3e")
    , overflow Css.hidden
    , position relative
    , minWidth (px 220)
    , maxWidth (px 1600)
    , Css.width (px width)
    ]


handleStyle : List Style
handleStyle =
    [ position absolute
    , right (px 0)
    , top (px 0)
    , Css.width (px 5)
    , Css.height (pct 100)
    , cursor ewResize
    , backgroundColor transparent
    , zIndex (int 10)
    , hover [ backgroundColor (hex "89b4fa44") ]
    ]


openStyle : List Style
openStyle =
    [ Css.width (px 16)
    , flexShrink (int 0)
    , backgroundColor (hex "1e2030")
    , borderStyle none
    , borderRight3 (px 1) solid (hex "2a2d3e")
    , color (hex "6272a4")
    , fontSize (rem 0.9)
    , cursor pointer
    , padding (px 0)
    , hover [ color (hex "cdd6f4"), backgroundColor (hex "2a2d3e") ]
    ]


headerStyle : List Style
headerStyle =
    [ displayFlex
    , justifyContent spaceBetween
    , alignItems center
    ]


tabsStyle : List Style
tabsStyle =
    [ displayFlex
    , Css.property "gap" "2px"
    ]


tabStyle : Bool -> List Style
tabStyle isActive =
    [ backgroundColor transparent
    , borderStyle none
    , color (hex "6272a4")
    , textTransform uppercase
    , letterSpacing (Css.em 0.05)
    , fontSize (rem 0.7)
    , padding2 (px 2) (px 6)
    , borderRadius (px 3)
    , cursor pointer
    , hover [ color (hex "cdd6f4") ]
    ]
        ++ (if isActive then
                [ color (hex "cdd6f4")
                , backgroundColor (hex "2a2d3e")
                ]

            else
                []
           )


closeStyle : List Style
closeStyle =
    [ backgroundColor transparent
    , borderStyle none
    , color (hex "6272a4")
    , fontSize (rem 1)
    , cursor pointer
    , padding2 (px 0) (px 2)
    , lineHeight (num 1)
    , hover [ color (hex "cdd6f4") ]
    ]


editorAndGraphStyle : List Style
editorAndGraphStyle =
    [ flex (int 1)
    , minHeight (px 0)
    , displayFlex
    , flexDirection column
    ]


codeWrapperStyle : List Style
codeWrapperStyle =
    [ flex (int 1)
    , minHeight (px 0)
    , position relative
    ]


graphHandleStyle : List Style
graphHandleStyle =
    [ flexShrink (int 0)
    , Css.height (px 5)
    , cursor nsResize
    , backgroundColor transparent
    , hover [ backgroundColor (hex "89b4fa44") ]
    ]


graphContainerStyle : Float -> List Style
graphContainerStyle graphHeight =
    [ flexShrink (int 0)
    , displayFlex
    , minHeight (px 0)
    , Css.height (px graphHeight)
    ]


graphViewStyle : List Style
graphViewStyle =
    [ flex (int 1)
    , minHeight (px 0)
    , position relative
    , displayFlex
    , backgroundColor (hex "1a1d2e")
    ]


conductorStyle : List Style
conductorStyle =
    [ displayFlex
    , flexDirection column
    , Css.property "gap" "6px"
    , flex (int 1)
    , minHeight (px 0)
    ]


conductorBpmStyle : List Style
conductorBpmStyle =
    [ displayFlex
    , alignItems center
    , Css.property "gap" "6px"
    , flexShrink (int 0)
    ]


conductorBpmLabelStyle : List Style
conductorBpmLabelStyle =
    [ fontSize (rem 0.7)
    , color (hex "6272a4")
    , textTransform uppercase
    , letterSpacing (Css.em 0.05)
    ]


conductorBpmInputStyle : List Style
conductorBpmInputStyle =
    [ Css.width (px 60)
    , backgroundColor (hex "1e2030")
    , border3 (px 1) solid (hex "2a2d3e")
    , color (hex "cdd6f4")
    , fontFamilies [ "monospace" ]
    , fontSize (rem 0.7)
    , padding2 (px 2) (px 4)
    , borderRadius (px 3)
    , focus
        [ outline none
        , borderColor (hex "89b4fa")
        ]
    ]


conductorListStyle : List Style
conductorListStyle =
    [ displayFlex
    , flexDirection column
    , Css.property "gap" "2px"
    , flexShrink (int 0)
    , maxHeight (pct 40)
    , overflowY auto
    ]


conductorEmptyStyle : List Style
conductorEmptyStyle =
    [ fontSize (rem 0.7)
    , color (hex "6272a4")
    , padding4 (px 4) (px 0) (px 4) (px 0)
    ]


conductorItemStyle : Bool -> List Style
conductorItemStyle isSelected =
    [ backgroundColor transparent
    , borderStyle none
    , textAlign left
    , color (hex "cdd6f4")
    , fontFamilies [ "monospace" ]
    , fontSize (rem 0.7)
    , padding4 (px 4) (px 6) (px 4) (px 6)
    , borderRadius (px 3)
    , cursor pointer
    , hover [ backgroundColor (hex "2a2d3e") ]
    ]
        ++ (if isSelected then
                [ backgroundColor (hex "313244") ]

            else
                []
           )


view : Model -> Html Msg
view model =
    if model.isCollapsed then
        button
            [ css openStyle
            , Events.onClick ToggleCollapse
            , title "Abrir score"
            ]
            [ text "›" ]

    else
        div
            [ css (panelStyle model.width) ]
            [ header model
            , case model.tab of
                ScoreTab ->
                    scoreTab model

                ConductorTab ->
                    conductorTab model
            , div
                [ css handleStyle
                , Events.preventDefaultOn "mousedown"
                    (Decode.map
                        (\x -> ( StartWidthResize x, True ))
                        (Decode.field "clientX" Decode.float)
                    )
                ]
                []
            ]


header : Model -> Html Msg
header model =
    div
        [ css headerStyle ]
        [ div
            [ css tabsStyle ]
            [ tabButton ScoreTab "score" model
            , tabButton ConductorTab "conductor" model
            ]
        , button
            [ css closeStyle
            , Events.onClick ToggleCollapse
            , title "Fechar score"
            ]
            [ text "×" ]
        ]


tabButton : Tab -> String -> Model -> Html Msg
tabButton target label model =
    button
        [ css (tabStyle (model.tab == target))
        , Events.onClick (TabChange target)
        ]
        [ text label ]


scoreTab : Model -> Html Msg
scoreTab model =
    div
        [ css editorAndGraphStyle ]
        [ div
            [ css codeWrapperStyle ]
            [ div
                [ id editorId
                , css [ Css.height (pct 100) ]
                ]
                []
            ]
        , div
            [ css graphHandleStyle
            , Events.preventDefaultOn "mousedown"
                (Decode.map
                    (\y -> ( StartGraphResize y, True ))
                    (Decode.field "clientY" Decode.float)
                )
            ]
            []
        , div
            [ css (graphContainerStyle model.graphHeight) ]
            [ GraphView.view { onRedraw = RequestGraphRedraw } model.graphState ]
        ]


conductorTab : Model -> Html Msg
conductorTab model =
    div
        [ css conductorStyle ]
        [ div
            [ css conductorBpmStyle ]
            [ span
                [ css conductorBpmLabelStyle ]
                [ text "bpm" ]
            , input
                [ css conductorBpmInputStyle
                , type_ "number"
                , Html.Styled.Attributes.min "1"
                , value model.bpm
                , Events.onInput BpmChange
                ]
                []
            ]
        , div
            [ css conductorListStyle ]
            [ button
                [ css (conductorItemStyle False)
                , Events.onClick NoOp
                ]
                [ text "global" ]
            , span
                [ css conductorEmptyStyle ]
                [ text "nenhum instrumento encontrado" ]
            ]
        ]
