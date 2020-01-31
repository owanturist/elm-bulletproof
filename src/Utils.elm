module Utils exposing (duplicates, ifelse, nonBlank, onSpaceOrEnter)

import AVL.Set as Set
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


duplicates : (a -> comparable) -> List a -> Maybe (List a)
duplicates toKey list =
    let
        ( _, result ) =
            List.foldr
                (\element ( uniq, acc ) ->
                    if Set.member (toKey element) uniq then
                        ( uniq, element :: acc )

                    else
                        ( Set.insert (toKey element) uniq, acc )
                )
                ( Set.empty, [] )
                list
    in
    if List.isEmpty result then
        Nothing

    else
        Just result


nonBlank : String -> Maybe String
nonBlank str =
    case String.trim str of
        "" ->
            Nothing

        trimmed ->
            Just trimmed
