module Checkbox exposing (checkbox)

import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attributes


checkbox : List (Html.Attribute msg) -> Html msg
checkbox attributes =
    input
        (Attributes.type_ "checkbox"
            :: Attributes.tabindex 0
            :: attributes
        )
        []
