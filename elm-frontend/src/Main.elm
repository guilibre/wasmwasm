module Main exposing (main)

import App.InstrumentTabs as InstrumentTabs
import App.Score.ScorePanel as ScorePanel
import App.Sidebar as Sidebar
import App.StatusBar as StatusBar
import Browser
import Css exposing (..)
import Css.Global exposing (body, each, global, html, selector)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Ports


type alias Model =
    { sideBar : Sidebar.Model
    , scorePanel : ScorePanel.Model
    , instrumentTabs : InstrumentTabs.Model
    , isPlaying : Bool
    , cpuLoad : Float
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( sideBar, sidebarCmd ) =
            Sidebar.init

        ( scorePanel, scorePanelCmd ) =
            ScorePanel.init
    in
    ( { sideBar = sideBar
      , scorePanel = scorePanel
      , instrumentTabs = InstrumentTabs.init
      , isPlaying = False
      , cpuLoad = 0
      , error = Nothing
      }
    , Cmd.batch
        [ Cmd.map SidebarMsg sidebarCmd
        , Cmd.map ScorePanelMsg scorePanelCmd
        ]
    )


type Msg
    = TogglePlay
    | Export
    | Import
    | InstrumentTabsMsg InstrumentTabs.Msg
    | SidebarMsg Sidebar.Msg
    | ScorePanelMsg ScorePanel.Msg
    | AudioCpu Float
    | AudioPlaying Bool
    | AudioError (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePlay ->
            if model.isPlaying then
                ( model, Ports.audioStop () )

            else
                ( model
                , Ports.audioPlay
                    { bpm = ScorePanel.bpmValue model.scorePanel
                    , instruments = List.map .id model.instrumentTabs.instruments
                    , scoreSource = model.scorePanel.source
                    }
                )

        AudioCpu load ->
            ( { model | cpuLoad = load }, Cmd.none )

        AudioPlaying playing ->
            ( { model | isPlaying = playing }, Cmd.none )

        AudioError maybeError ->
            ( { model | error = maybeError }, Cmd.none )

        Export ->
            ( model, Cmd.none )

        Import ->
            ( model, Cmd.none )

        InstrumentTabsMsg subMsg ->
            ( { model | instrumentTabs = InstrumentTabs.update subMsg model.instrumentTabs }, Cmd.none )

        SidebarMsg subMsg ->
            let
                ( nextSidebar, cmd ) =
                    Sidebar.update subMsg model.sideBar
            in
            ( { model | sideBar = nextSidebar }, Cmd.map SidebarMsg cmd )

        ScorePanelMsg subMsg ->
            let
                ( nextScorePanel, cmd ) =
                    ScorePanel.update subMsg model.scorePanel
            in
            ( { model | scorePanel = nextScorePanel }, Cmd.map ScorePanelMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SidebarMsg (Sidebar.subscriptions model.sideBar)
        , Sub.map ScorePanelMsg (ScorePanel.subscriptions model.scorePanel)
        , Ports.audioCpu AudioCpu
        , Ports.audioPlaying AudioPlaying
        , Ports.audioError AudioError
        ]


toolbarButton : List Style
toolbarButton =
    [ padding2 (rem 0.25) (rem 0.75)
    , backgroundColor (hex "#263238")
    , color (hex "#cdd6f4")
    , border3 (px 1) solid (hex "#37474f")
    , borderRadius (px 4)
    , cursor pointer
    , fontFamilies [ "monospace" ]
    , fontSize (rem 0.85)
    ]


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , Css.height (pct 100)
            , backgroundColor (hex "#1a1d2e")
            , color (hex "#cdd6f4")
            , fontFamilies [ "monospace" ]
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                , Css.property "gap" "0.5rem"
                , padding2 (rem 0.4) (rem 0.8)
                , backgroundColor (hex "#13151f")
                , borderBottom3 (px 1) solid (hex "#2a2d40")
                , flexShrink (int 0)
                ]
            ]
            [ span
                [ css
                    [ fontWeight bold
                    , color (hex "#89ddff")
                    , marginRight (rem 0.5)
                    ]
                ]
                [ text "wasmwasm" ]
            , button
                [ css toolbarButton, onClick TogglePlay ]
                [ text
                    (if model.isPlaying then
                        "Stop"

                     else
                        "Play"
                    )
                ]
            , button [ css toolbarButton, onClick Export ] [ text "Export" ]
            , button [ css toolbarButton, onClick Import ] [ text "Import" ]
            , case model.error of
                Just message ->
                    span
                        [ css
                            [ color (hex "#f07178")
                            , fontSize (rem 0.8)
                            , marginLeft (rem 0.5)
                            ]
                        ]
                        [ text message ]

                Nothing ->
                    text ""
            ]
        , div
            [ css
                [ flex (int 1)
                , minHeight (px 0)
                , displayFlex
                ]
            ]
            [ Html.Styled.map ScorePanelMsg (ScorePanel.view model.scorePanel)
            , div
                [ css
                    [ flex (int 1)
                    , minWidth (px 0)
                    , displayFlex
                    , position relative
                    ]
                ]
                [ div
                    [ css
                        [ flex (int 1)
                        , minWidth (px 0)
                        , displayFlex
                        , flexDirection column
                        , position relative
                        ]
                    ]
                    [ Html.Styled.map InstrumentTabsMsg (InstrumentTabs.view model.instrumentTabs)
                    , div [ css [ flex (int 1) ] ] []
                    , StatusBar.view { isPlaying = model.isPlaying, cpuLoad = model.cpuLoad }
                    ]
                , Html.Styled.map SidebarMsg (Sidebar.view model.sideBar)
                ]
            ]
        ]


globalStyles : Html msg
globalStyles =
    global
        [ each [ html, body ]
            [ margin (px 0)
            , padding (px 0)
            , Css.height (pct 100)
            , overflow Css.hidden
            ]
        , selector "#app"
            [ Css.height (pct 100) ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { title = "wasmwasm"
                , body = [ toUnstyled globalStyles, toUnstyled (view model) ]
                }
        , subscriptions = subscriptions
        }
