module Utils exposing (duplicates, ifelse, nonBlank, onSpaceOrEnter)

import AVL.Dict as Dict
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


duplicates : (a -> Maybe comparable) -> List a -> List ( comparable, Int )
duplicates toKey list =
    let
        ( counts, keys ) =
            List.foldr
                (\element ( counter, acc ) ->
                    case toKey element of
                        Nothing ->
                            ( counter, acc )

                        Just key ->
                            case Dict.get key counter of
                                Nothing ->
                                    ( Dict.insert key 1 counter
                                    , acc
                                    )

                                Just count ->
                                    ( Dict.insert key (count + 1) counter
                                    , key :: acc
                                    )
                )
                ( Dict.empty, [] )
                list
    in
    List.filterMap
        (\key -> Maybe.map (Tuple.pair key) (Dict.get key counts))
        keys


nonBlank : String -> Maybe String
nonBlank str =
    case String.trim str of
        "" ->
            Nothing

        trimmed ->
            Just trimmed
