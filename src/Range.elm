module Range exposing (css, range)

import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Style


css : Style.Sheet
css =
    Style.sheet
        [ range__root
        , range__border
        , range__input
        ]


range__root : Style.Selector
range__root =
    Style.class "range__root"
        [ Style.rule "display" "flex"
        , Style.rule "align-items" "center"
        , Style.rule "width" "100%"
        , Style.rule "font-family" "monospace"
        ]


range__border : Style.Selector
range__border =
    Style.class "range__border"
        [ Style.rule "padding" "0 5px"
        , Style.rule "font-size" "12px"
        , Style.rule "white-space" "no-wrap"
        ]


range__input : Style.Selector
range__input =
    Style.class "range__input"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "table-cell"
        , Style.rule "flex-grow" "1"
        , Style.rule "padding" "5px"
        , Style.rule "height" "11px"
        , Style.rule "border" "1px solid #f7f4f4"
        , Style.rule "border-radius" "2px"
        , Style.rule "color" "#444"
        , Style.rule "outline" "none"
        ]


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
    div
        [ Style.className range__root ]
        [ span
            [ Style.className range__border ]
            [ text (trailingZeros stepStr minStr)
            ]
        , input
            [ Style.className range__input
            , Attributes.type_ "range"
            , Attributes.tabindex 0
            , Attributes.name name
            , Attributes.min minStr
            , Attributes.max maxStr
            , Attributes.step stepStr
            , Attributes.value valueStr
            , Events.onInput msg
            ]
            []
        , span
            [ Style.className range__border ]
            [ text (trailingZeros stepStr valueStr ++ " / " ++ trailingZeros stepStr maxStr)
            ]
        ]
