module Range exposing (css, range)

import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy
import Style


css : Style.Sheet
css =
    Style.elements
        [ range__root
        , range__border
        , range__input
        ]


range__root : Style.Element
range__root =
    Style.el "range__root"
        [ Style.rule "display" "flex"
        , Style.rule "align-items" "center"
        , Style.rule "width" "100%"
        , Style.rule "font-family" "monospace"
        ]


range__border : Style.Element
range__border =
    Style.el "range__border"
        [ Style.rule "padding" "0 5px"
        , Style.rule "font-size" "12px"
        , Style.rule "white-space" "no-wrap"
        ]


range__input : Style.Element
range__input =
    Style.el "range__input"
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


rangeHelp : String -> String -> String -> String -> String -> Html String
rangeHelp name min max step value =
    div
        [ Style.class range__root ]
        [ span
            [ Style.class range__border ]
            [ text (trailingZeros step min)
            ]
        , input
            [ Style.class range__input
            , Attributes.type_ "range"
            , Attributes.tabindex 0
            , Attributes.name name
            , Attributes.min min
            , Attributes.max max
            , Attributes.step step
            , Attributes.value value
            , Events.onInput identity
            ]
            []
        , span
            [ Style.class range__border ]
            [ text (trailingZeros step value ++ " / " ++ trailingZeros step max)
            ]
        ]


range :
    String
    -> (number -> String)
    ->
        { min : number
        , max : number
        , step : number
        , value : number
        }
    -> Html String
range name numToString { min, max, step, value } =
    Lazy.lazy5 rangeHelp
        name
        (numToString min)
        (numToString max)
        (numToString step)
        (numToString (clamp min max value))
