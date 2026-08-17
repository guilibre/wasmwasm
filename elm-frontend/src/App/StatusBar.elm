module App.StatusBar exposing (Model, view)

import Css exposing (..)
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (css)


type alias Model =
    { isPlaying : Bool
    , cpuLoad : Float
    }


type Level
    = Normal
    | Warn
    | Danger


levelFor : Int -> Level
levelFor pct =
    if pct >= 90 then
        Danger

    else if pct >= 50 then
        Warn

    else
        Normal


levelColor : Level -> Css.Color
levelColor level =
    case level of
        Normal ->
            hex "#50fa7b"

        Warn ->
            hex "#f1fa8c"

        Danger ->
            hex "#ff5555"


view : Model -> Html msg
view model =
    let
        pct =
            Basics.round (model.cpuLoad * 100)

        barPct =
            Css.pct (toFloat (min 100 pct))

        level =
            levelFor pct
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , property "gap" "0.5rem"
            , padding2 (rem 0.2) (rem 0.8)
            , backgroundColor (hex "#13151f")
            , borderTop3 (px 1) solid (hex "#2a2d40")
            , flexShrink (int 0)
            , fontFamilies [ "monospace" ]
            , fontSize (rem 0.7)
            , color (hex "#6272a4")
            ]
        ]
        [ span []
            [ text
                (if model.isPlaying then
                    "playing"

                 else
                    "stopped"
                )
            ]
        , div
            [ css
                [ displayFlex
                , alignItems center
                , property "gap" "0.5rem"
                ]
            ]
            [ div
                [ css
                    [ width (px 80)
                    , height (px 6)
                    , backgroundColor (hex "#2a2d40")
                    , borderRadius (px 3)
                    , overflow hidden
                    ]
                ]
                [ div
                    [ css
                        [ height pct100
                        , width barPct
                        , backgroundColor (levelColor level)
                        , property "transition" "width 0.1s linear"
                        ]
                    ]
                    []
                ]
            , span
                [ css
                    [ minWidth (em 4.5)
                    , textAlign right
                    ]
                ]
                [ text ("cpu " ++ String.fromInt pct ++ "%") ]
            ]
        ]


pct100 : Css.Pct
pct100 =
    Css.pct 100
