module Counter exposing (Model, Msg, initial, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events



-- M O D E L


type alias Model =
    Int


foo =
    "String"


initial : Model
initial =
    0



-- U P D A T E


type Msg
    = Decrement
    | Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1



-- V I E W


view : Model -> Html Msg
view model =
    div []
        [ button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Decrement
            ]
            [ text "-"
            ]
        , text (String.fromInt model)
        , button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick Increment
            ]
            [ text "+"
            ]
        ]
