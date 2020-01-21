module Range exposing (range)

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (style)
import Html.Events


viewContainer : List (Html msg) -> Html msg
viewContainer =
    div
        [ style "display" "flex"
        , style "-webkit-box-align" "center"
        , style "align-items" "center"
        , style "width" "100%"
        , style "font-family" "monospace"
        ]


viewBorder : List (Html msg) -> Html msg
viewBorder =
    span
        [ style "padding" "0 5px"
        , style "font-size" "12px"
        , style "white-space" "nowrap"
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
    viewContainer
        [ viewBorder [ text (trailingZeros stepStr minStr) ]
        , input
            [ style "box-sizing" "border-box"
            , style "padding" "5px"
            , style "height" "11px"
            , style "color" "#444"
            , style "display" "table-cell"
            , style "-webkit-box-flex" "1"
            , style "flex-grow" "1"
            , style "outline" "none"
            , style "border" "1px solid #f7f4f4"
            , style "border-radius" "2px"

            --
            , Html.Attributes.type_ "range"
            , Html.Attributes.name name
            , Html.Attributes.min minStr
            , Html.Attributes.max maxStr
            , Html.Attributes.step stepStr
            , Html.Attributes.value valueStr

            --
            , Html.Events.onInput msg
            ]
            []
        , viewBorder [ text (trailingZeros stepStr valueStr ++ " / " ++ trailingZeros stepStr maxStr) ]
        ]
