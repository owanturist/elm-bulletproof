module Utils exposing (Viewport, ifelse, notClosest, onSpaceOrEnter, px, textCode)

import Css
import DOM
import Html.Styled as Html exposing (Html, code, styled, text)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Palette


type alias Viewport =
    { width : Int
    , height : Int
    }


px : Int -> String
px n =
    String.fromInt n ++ "px"


textCode : String -> Html msg
textCode =
    styled code
        [ Css.display Css.inlineBlock
        , Css.padding2 (Css.px 2) (Css.px 4)
        , Css.backgroundColor Palette.cloud
        , Css.letterSpacing (Css.em 0.05)
        , Css.fontFamily Css.monospace
        , Css.lineHeight (Css.px 18)
        ]
        []
        << List.singleton
        << text


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
                DOM.parentElement (closest className decoder)
        )
        DOM.className


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
        |> DOM.target
