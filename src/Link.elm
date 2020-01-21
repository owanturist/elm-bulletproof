module Link exposing (link)

import Html exposing (Html, a)
import Html.Attributes exposing (style)
import Palette
import Router


styles : List (Html.Attribute msg)
styles =
    [ style "color" Palette.aqua
    ]


link : Router.Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link route attributes children =
    a (Html.Attributes.href (Router.toString route) :: styles ++ attributes) children
