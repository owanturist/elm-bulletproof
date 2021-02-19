module Utils exposing (Viewport, ifelse, notClosest, onSpaceOrEnter, px)

import Html.Styled as Html
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)


type alias Viewport =
    { width : Int
    , height : Int
    }


px : Int -> String
px n =
    String.fromInt n ++ "px"


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


containsClass : String -> String -> Bool
containsClass className classList =
    List.member className (String.split " " classList)


closest : String -> Decoder node -> Decoder node
closest className decoder =
    Decode.andThen
        (\classList ->
            if containsClass className classList then
                decoder

            else
                Decode.field "parentElement" (closest className decoder)
        )
        (Decode.field "className" Decode.string)


notClosest : String -> msg -> Decoder msg
notClosest className msg =
    Decode.andThen
        (\withClassName ->
            if withClassName then
                Decode.fail ("Class `" ++ className ++ "` exists in closest nodes.")

            else
                Decode.succeed msg
        )
        (Decode.oneOf
            [ closest className (Decode.succeed True)
            , Decode.succeed False
            ]
        )
        |> Decode.field "target"
