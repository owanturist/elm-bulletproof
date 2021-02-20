module Stories.Icon exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html.Styled exposing (Html, code, div, text)
import Html.Styled.Attributes exposing (style)
import Icon


viewContainer : String -> List (Html msg) -> Html msg
viewContainer color =
    div
        [ style "display" "flex"
        , style "flex-flow" "row wrap"
        , style "color" color
        ]


viewBlock : ( String, Html msg ) -> Html msg
viewBlock ( name, child ) =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "width" "150px"
        , style "height" "100px"
        ]
        [ child
        , code
            [ style "margin-top" "8px"
            , style "font-size" "12px"
            ]
            [ text name ]
        ]


story : Bulletproof.Story
story =
    Bulletproof.story "Icon"
        (\color ->
            [ ( "folder", Icon.folder )
            , ( "folderOpen", Icon.folderOpen )
            , ( "folderEmpty", Icon.folderEmpty )
            , ( "folderEmptyOpen", Icon.folderEmptyOpen )
            , ( "elm", Icon.elm )
            , ( "tools", Icon.tools )
            , ( "bars", Icon.bars )
            ]
                |> List.map viewBlock
                |> viewContainer color.hex
        )
        |> Bulletproof.Knob.color "Color" "#1ea5fd"
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
