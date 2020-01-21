module Link exposing (link)

import Html exposing (Html, a)
import Html.Attributes
import Palette
import Router


styles : List ( String, String )
styles =
    [ ( "color", Palette.aqua )
    ]


link : Router.Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link route attributes children =
    a
        (Html.Attributes.href (Router.toString route)
            :: Html.Attributes.tabindex 0
            :: List.foldl (\( key, value ) acc -> Html.Attributes.style key value :: acc)
                attributes
                styles
        )
        children
