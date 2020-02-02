module Utils exposing (ifelse, nonBlank, onSpaceOrEnter, plural)

import Html.Styled as Html
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)


ifelse : Bool -> x -> x -> x
ifelse bool onTrue onFalse =
    if bool then
        onTrue

    else
        onFalse


keyDecoder : List Int -> msg -> Decoder ( msg, Bool )
keyDecoder keys msg =
    Decode.andThen
        (\keyCode ->
            if List.member keyCode keys then
                Decode.succeed ( msg, True )

            else
                Decode.fail "Ignore that"
        )
        Events.keyCode


onSpaceOrEnter : msg -> Html.Attribute msg
onSpaceOrEnter =
    Events.preventDefaultOn "keypress" << keyDecoder [ 13, 32 ]


nonBlank : String -> Maybe String
nonBlank str =
    case String.trim str of
        "" ->
            Nothing

        trimmed ->
            Just trimmed


plural : String -> Int -> String
plural word n =
    if n == 1 then
        word

    else
        word ++ "s"
