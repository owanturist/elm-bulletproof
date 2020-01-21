module Checkbox exposing (checkbox)

import Html exposing (Html, input)
import Html.Attributes


checkbox : List (Html.Attribute msg) -> Html msg
checkbox attributes =
    input
        (Html.Attributes.type_ "checkbox"
            :: Html.Attributes.tabindex 0
            :: attributes
        )
        []
