module Link exposing (link)

import Css
import Html.Styled as Html exposing (Html, a, styled)
import Html.Styled.Attributes as Attributes
import Palette
import Router


styledA : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledA =
    styled a
        [ Css.color Palette.aqua
        ]


link : Router.Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link route attributes children =
    styledA
        (Attributes.href (Router.toString route)
            :: Attributes.tabindex 0
            :: attributes
        )
        children
