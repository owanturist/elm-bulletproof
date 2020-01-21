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
        val =
            clamp min max value
    in
    viewContainer
        [ viewBorder [ text (numToString min) ]
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
            , Html.Attributes.min (numToString min)
            , Html.Attributes.max (numToString max)
            , Html.Attributes.step (numToString step)
            , Html.Attributes.value (numToString val)

            --
            , Html.Events.onInput msg
            ]
            []
        , viewBorder [ text (numToString val ++ " / " ++ numToString max) ]
        ]
