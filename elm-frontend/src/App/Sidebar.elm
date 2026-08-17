module App.Sidebar exposing (..)

import Browser.Events
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Ports


waveformId : String
waveformId =
    "sidebar-waveform"


spectrumId : String
spectrumId =
    "sidebar-spectrum"


minWidthPx : Float
minWidthPx =
    100


maxWidthPx : Float
maxWidthPx =
    600


defaultWidth : Float
defaultWidth =
    100


type alias DragState =
    { startX : Float
    , startWidth : Float
    }


type SetupState
    = NeedsSetup
    | SetupSent


type alias Model =
    { isCollapsed : Bool
    , width : Float
    , drag : Maybe DragState
    , canvasSetup : SetupState
    }


init : ( Model, Cmd Msg )
init =
    maybeSendSetup
        { isCollapsed = False
        , width = defaultWidth
        , drag = Nothing
        , canvasSetup = NeedsSetup
        }
        Cmd.none


type Msg
    = ToggleCollapse
    | StartResize Float
    | MouseMove Float
    | MouseUp
    | Wheel Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        next =
            case msg of
                ToggleCollapse ->
                    ( { model | isCollapsed = not model.isCollapsed, canvasSetup = NeedsSetup }, Cmd.none )

                StartResize x ->
                    ( { model | drag = Just { startX = x, startWidth = model.width } }, Cmd.none )

                MouseMove x ->
                    case model.drag of
                        Just { startX, startWidth } ->
                            let
                                delta =
                                    startX - x

                                width =
                                    clamp minWidthPx maxWidthPx (startWidth + delta)
                            in
                            ( { model | width = width }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                MouseUp ->
                    ( { model | drag = Nothing }, Cmd.none )

                Wheel deltaY ->
                    ( model, Ports.sidebarWheel deltaY )
    in
    maybeSendSetup (Tuple.first next) (Tuple.second next)


maybeSendSetup : Model -> Cmd Msg -> ( Model, Cmd Msg )
maybeSendSetup model cmd =
    if not model.isCollapsed && model.canvasSetup == NeedsSetup then
        ( { model | canvasSetup = SetupSent }
        , Cmd.batch
            [ cmd
            , Ports.sidebarSetup { waveformId = waveformId, spectrumId = spectrumId }
            ]
        )

    else
        ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map MouseMove (Decode.field "clientX" Decode.float))
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

        Nothing ->
            Sub.none


onWheelPrevent : (Float -> msg) -> Attribute msg
onWheelPrevent tagger =
    Events.preventDefaultOn "wheel"
        (Decode.map
            (\deltaY -> ( tagger deltaY, True ))
            (Decode.field "deltaY" Decode.float)
        )


sidebarStyle : Float -> List Style
sidebarStyle width =
    [ flexShrink (int 0)
    , displayFlex
    , flexDirection column
    , Css.property "gap" "6px"
    , padding4 (px 8) (px 8) (px 8) (px 12)
    , backgroundColor (hex "#13151f")
    , borderLeft3 (px 1) solid (hex "#2a2d3e")
    , overflow Css.hidden
    , position relative
    , minWidth (px minWidthPx)
    , maxWidth (px maxWidthPx)
    , Css.width (px width)
    ]


handleStyle : List Style
handleStyle =
    [ position absolute
    , left (px 0)
    , top (px 0)
    , Css.width (px 5)
    , Css.height (pct 100)
    , cursor ewResize
    , backgroundColor transparent
    , zIndex (int 10)
    , hover [ backgroundColor (hex "#89b4fa44") ]
    ]


openStyle : List Style
openStyle =
    [ Css.width (px 16)
    , flexShrink (int 0)
    , backgroundColor (hex "#1e2030")
    , borderStyle none
    , borderLeft3 (px 1) solid (hex "#2a2d3e")
    , color (hex "#6272a4")
    , fontSize (rem 0.9)
    , cursor pointer
    , padding (px 0)
    , hover [ color (hex "#cdd6f4"), backgroundColor (hex "#2a2d3e") ]
    ]


headerStyle : List Style
headerStyle =
    [ displayFlex
    , justifyContent flexEnd
    ]


closeStyle : List Style
closeStyle =
    [ backgroundColor transparent
    , borderStyle none
    , color (hex "#6272a4")
    , fontSize (rem 1)
    , cursor pointer
    , padding2 (px 0) (px 2)
    , lineHeight (num 1)
    , hover [ color (hex "#cdd6f4") ]
    ]


labelStyle : List Style
labelStyle =
    [ fontSize (rem 0.7)
    , color (hex "#6272a4")
    , textTransform uppercase
    , letterSpacing (Css.em 0.05)
    ]


canvasStyle : Int -> List Style
canvasStyle flexValue =
    [ Css.width (pct 100)
    , display block
    , borderRadius (px 3)
    , cursor crosshair
    , minHeight (px 0)
    , flex (int flexValue)
    ]


view : Model -> Html Msg
view model =
    if model.isCollapsed then
        button
            [ css openStyle
            , Events.onClick ToggleCollapse
            ]
            [ text "‹" ]

    else
        div
            [ css (sidebarStyle model.width) ]
            [ div
                [ css handleStyle
                , Events.preventDefaultOn "mousedown"
                    (Decode.map
                        (\x -> ( StartResize x, True ))
                        (Decode.field "clientX" Decode.float)
                    )
                ]
                []
            , div [ css headerStyle ]
                [ button
                    [ css closeStyle
                    , Events.onClick ToggleCollapse
                    ]
                    [ text "×" ]
                ]
            , span [ css labelStyle ] [ text "waveform" ]
            , canvas
                [ id waveformId
                , css (canvasStyle 3)
                , onWheelPrevent Wheel
                ]
                []
            , span [ css labelStyle ] [ text "spectrum" ]
            , canvas
                [ id spectrumId
                , css (canvasStyle 7)
                ]
                []
            ]
