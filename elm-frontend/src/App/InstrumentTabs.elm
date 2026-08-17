module App.InstrumentTabs exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Set


type alias Instrument =
    { id : String }


type View
    = InstrumentView
    | GlobalView


type alias Model =
    { instruments : List Instrument
    , activeId : Maybe String
    , view : View
    , editingId : Maybe String
    , nameDraft : String
    }


init : Model
init =
    { instruments = []
    , activeId = Nothing
    , view = GlobalView
    , editingId = Nothing
    , nameDraft = ""
    }


type Msg
    = NoOp
    | Add
    | Remove String
    | Rename String String
    | SetActive String
    | ViewChange View
    | StartRename String
    | UpdateDraft String
    | CommitRename
    | CancelRename


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Add ->
            let
                n =
                    List.length model.instruments + 1

                id =
                    uniqueId ("instrument" ++ String.fromInt n) model.instruments Nothing

                instruments =
                    model.instruments ++ [ { id = id } ]
            in
            { model
                | instruments = instruments
                , activeId = Just id
                , view = InstrumentView
            }

        Remove id ->
            let
                remaining =
                    List.filter (\i -> i.id /= id) model.instruments

                activeId =
                    if model.activeId == Just id then
                        case List.reverse remaining of
                            last :: _ ->
                                Just last.id

                            [] ->
                                Nothing

                    else
                        model.activeId
            in
            { model | instruments = remaining, activeId = activeId }

        Rename id newName ->
            let
                unique =
                    uniqueId newName model.instruments (Just id)

                instruments =
                    List.map
                        (\i ->
                            if i.id == id then
                                { i | id = unique }

                            else
                                i
                        )
                        model.instruments
            in
            { model | instruments = instruments, editingId = Nothing }

        SetActive id ->
            { model | activeId = Just id, view = InstrumentView }

        ViewChange newView ->
            { model | view = newView }

        StartRename id ->
            { model | editingId = Just id, nameDraft = id }

        UpdateDraft draft ->
            { model | nameDraft = draft }

        CommitRename ->
            case model.editingId of
                Just id ->
                    let
                        trimmed =
                            String.trim model.nameDraft
                    in
                    if String.isEmpty trimmed then
                        { model | editingId = Nothing }

                    else
                        update (Rename id trimmed) model

                Nothing ->
                    model

        CancelRename ->
            { model | editingId = Nothing }


uniqueId : String -> List Instrument -> Maybe String -> String
uniqueId desired existing ignoreId =
    let
        taken =
            List.filter (\i -> Just i.id /= ignoreId) existing
                |> List.map .id
                |> Set.fromList
    in
    if not (Set.member desired taken) then
        desired

    else
        uniqueIdSuffix desired taken 2


uniqueIdSuffix : String -> Set.Set String -> Int -> String
uniqueIdSuffix desired taken n =
    let
        candidate =
            desired ++ String.fromInt n
    in
    if not (Set.member candidate taken) then
        candidate

    else
        uniqueIdSuffix desired taken (n + 1)


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.map tagger Events.keyCode)


stopClick : Msg -> Attribute Msg
stopClick msg =
    Events.stopPropagationOn "click" (Decode.succeed ( msg, True ))


handleKey : Int -> Msg
handleKey code =
    if code == 13 then
        CommitRename

    else if code == 27 then
        CancelRename

    else
        NoOp


tabsStyle : List Style
tabsStyle =
    [ displayFlex
    , flexWrap noWrap
    , overflowX auto
    , alignItems stretch
    , minHeight (px 0)
    , Css.property "scrollbar-width" "none"
    , pseudoElement "::-webkit-scrollbar" [ display none ]
    ]


tabStyle : Bool -> List Style
tabStyle isActive =
    let
        base =
            [ displayFlex
            , alignItems center
            , Css.property "gap" "0.25rem"
            , padding2 (rem 0.35) (rem 0.6)
            , cursor pointer
            , color (hex "#6272a4")
            , borderBottom3 (px 2) solid transparent
            , whiteSpace noWrap
            , fontSize (rem 0.8)
            , Css.property "user-select" "none"
            , hover [ color (hex "#cdd6f4") ]
            ]
    in
    if isActive then
        base ++ [ color (hex "#cdd6f4"), borderBottomColor (hex "#89b4fa") ]

    else
        base


tabNameStyle : List Style
tabNameStyle =
    [ maxWidth (px 100)
    , overflow Css.hidden
    , textOverflow ellipsis
    ]


tabInputStyle : List Style
tabInputStyle =
    [ Css.width (px 80)
    , backgroundColor transparent
    , borderStyle none
    , borderBottom3 (px 1) solid (hex "#89b4fa")
    , color (hex "#cdd6f4")
    , fontFamilies [ "monospace" ]
    , fontSize (rem 0.8)
    , Css.property "outline" "none"
    , padding (px 0)
    ]


removeStyle : List Style
removeStyle =
    [ backgroundColor transparent
    , borderStyle none
    , color (hex "#45475a")
    , cursor pointer
    , fontSize (rem 0.9)
    , padding2 (px 0) (rem 0.1)
    , lineHeight (num 1)
    , hover [ color (hex "#f07178") ]
    ]


addStyle : List Style
addStyle =
    [ backgroundColor transparent
    , borderStyle none
    , color (hex "#6272a4")
    , cursor pointer
    , fontSize (rem 1)
    , padding2 (rem 0.35) (rem 0.6)
    , hover [ color (hex "#89b4fa") ]
    ]


view : Model -> Html Msg
view model =
    let
        resolvedId =
            if model.view == GlobalView then
                Nothing

            else
                case model.activeId of
                    Just id ->
                        Just id

                    Nothing ->
                        List.head model.instruments |> Maybe.map .id
    in
    div [ css tabsStyle ]
        (globalTab (resolvedId == Nothing)
            :: (List.map (instrumentTab model resolvedId) model.instruments ++ [ addButton ])
        )


globalTab : Bool -> Html Msg
globalTab isActive =
    div
        [ css (tabStyle isActive)
        , Events.onClick (ViewChange GlobalView)
        ]
        [ span [ css tabNameStyle ] [ text "global" ] ]


instrumentTab : Model -> Maybe String -> Instrument -> Html Msg
instrumentTab model resolvedId instrument =
    let
        isActive =
            resolvedId == Just instrument.id
    in
    div
        [ css (tabStyle isActive)
        , Events.onClick (SetActive instrument.id)
        ]
        [ if model.editingId == Just instrument.id then
            input
                [ css tabInputStyle
                , autofocus True
                , value model.nameDraft
                , Events.onInput UpdateDraft
                , onKeyDown handleKey
                , Events.onBlur CommitRename
                , stopClick NoOp
                ]
                []

          else
            span
                [ css tabNameStyle
                , Events.onDoubleClick (StartRename instrument.id)
                ]
                [ text instrument.id ]
        , button
            [ css removeStyle
            , stopClick (Remove instrument.id)
            ]
            [ text "×" ]
        ]


addButton : Html Msg
addButton =
    button [ css addStyle, Events.onClick Add ] [ text "+" ]
