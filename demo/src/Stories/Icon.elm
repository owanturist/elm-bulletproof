module Stories.Icon exposing (story)

import Bulletproof
import Bulletproof.Knob
import Css
import Html.Styled exposing (Html, div, span, styled, text)
import Icon


viewContainer : Css.Color -> List (Html msg) -> Html msg
viewContainer color =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.wrap
        , Css.color color
        ]
        []


viewBlock : ( String, Html msg ) -> Html msg
viewBlock ( name, child ) =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.width (Css.px 150)
        , Css.height (Css.px 100)
        ]
        []
        [ child
        , styled span
            [ Css.marginTop (Css.px 8)
            , Css.fontSize (Css.px 12)
            , Css.fontFamily Css.monospace
            ]
            []
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
            , ( "dockHorizontal", Icon.dockHorizontal )
            , ( "dockVertical", Icon.dockVertical )
            , ( "elm", Icon.elm )
            , ( "bordersBold", Icon.bordersBold )
            , ( "bordersThin", Icon.bordersThin )
            , ( "square", Icon.square )
            , ( "squareBordered", Icon.squareBordered )
            , ( "fill", Icon.fill )
            , ( "fillDrop", Icon.fillDrop )
            , ( "grid", Icon.grid )
            , ( "tools", Icon.tools )
            ]
                |> List.map viewBlock
                |> viewContainer (Css.hex color.hex)
                |> Bulletproof.fromElmCss
        )
        |> Bulletproof.Knob.color "Color" "#000"
