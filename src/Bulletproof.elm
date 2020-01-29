module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , folderOf
    , fromElmCss
    , fromElmUI
    , fromHtml
    , label
    , program
    , storyOf
    , todo
    )

import Element exposing (Element)
import Html
import Html.Styled
import Main
import Renderer
import Story exposing (Story)


type alias Renderer =
    Renderer.Renderer


fromElmCss : Html.Styled.Html msg -> Renderer
fromElmCss layout =
    Renderer.Renderer (Html.Styled.map (always ()) layout)


fromHtml : Html.Html msg -> Renderer
fromHtml layout =
    fromElmCss (Html.Styled.fromUnstyled layout)


fromElmUI : List Element.Option -> List (Element.Attribute msg) -> Element msg -> Renderer
fromElmUI options attributes element =
    fromHtml (Element.layoutWith { options = options } attributes element)


type alias Story =
    Story.Story Renderer


storyOf : String -> view -> Story.Story view
storyOf title view_ =
    Story.Single title
        { knobs = []
        , view = Ok (\_ -> view_)
        }


folderOf : String -> List Story -> Story
folderOf title stories =
    Story.Batch title stories


todo : String -> Story
todo title =
    Story.Todo title


label : String -> Story
label title =
    Story.Label title


type alias Program =
    Main.Program


program : List Story -> Program
program stories =
    Main.run stories
