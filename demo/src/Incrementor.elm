module Incrementor exposing (Model, Msg, initial, update, view)

import Html exposing (Html, button, text)
import Html.Attributes
import Html.Events



-- M O D E L


type alias Model =
    Int


initial : Model
initial =
    0



-- U P D A T E


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1



-- V I E W


view : Model -> Html Msg
view model =
    button
        [ Html.Attributes.type_ "button"
        , Html.Events.onClick Increment
        ]
        [ text (String.fromInt model)
        ]
