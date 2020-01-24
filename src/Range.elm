module Range exposing (range)

import Css
import Html.Styled as Html exposing (Html, div, input, span, styled, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


trailingZeros : String -> String -> String
trailingZeros step value =
    case String.split "." step of
        [ _, rightStep ] ->
            case String.split "." value of
                [ leftValue, rightValue ] ->
                    leftValue ++ "." ++ rightValue ++ String.repeat (max 0 (String.length rightStep - String.length rightValue)) "0"

                _ ->
                    value ++ "." ++ String.repeat (String.length rightStep) "0"

        _ ->
            value


styledContainer : List (Html msg) -> Html msg
styledContainer =
    styled div
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.width (Css.pct 100)
        , Css.fontFamily Css.monospace
        ]
        []


styledBorder : List (Html msg) -> Html msg
styledBorder =
    styled span
        [ Css.padding2 Css.zero (Css.px 5)
        , Css.fontSize (Css.px 12)
        , Css.whiteSpace Css.noWrap
        ]
        []


styledInput : List (Html.Attribute msg) -> Html msg
styledInput attributes =
    styled input
        [ Css.boxSizing Css.borderBox
        , Css.display Css.tableCell
        , Css.flexGrow (Css.int 1)
        , Css.padding (Css.px 5)
        , Css.height (Css.px 11)
        , Css.border3 (Css.px 1) Css.solid (Css.hex "#f7f4f4")
        , Css.borderRadius (Css.px 2)
        , Css.color (Css.hex "#444")
        , Css.outline Css.none
        ]
        attributes
        []


range :
    (String -> msg)
    -> String
    -> (number -> String)
    ->
        { min : number
        , max : number
        , step : number
        , value : number
        }
    -> Html msg
range msg name numToString { min, max, step, value } =
    let
        ( minStr, maxStr, stepStr ) =
            ( numToString min, numToString max, numToString step )

        valueStr =
            numToString (clamp min max value)
    in
    styledContainer
        [ styledBorder
            [ text (trailingZeros stepStr minStr)
            ]
        , styledInput
            [ Attributes.type_ "range"
            , Attributes.name name
            , Attributes.min minStr
            , Attributes.max maxStr
            , Attributes.step stepStr
            , Attributes.value valueStr
            , Events.onInput msg
            ]
        , styledBorder
            [ text (trailingZeros stepStr valueStr ++ " / " ++ trailingZeros stepStr maxStr)
            ]
        ]
