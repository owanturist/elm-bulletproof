module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , folder
    , fromElmCss
    , fromElmUI
    , fromHtml
    , label
    , program
    , story
    , todo
    )

import Element exposing (Element)
import Error
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
    Story.Story Error.Reason Renderer


story : String -> view -> Story.Story Error.Reason view
story title view =
    Story.Single title
        { knobs = []
        , view = always view
        }


folder : String -> List Story -> Story
folder title stories =
    Story.Batch title stories


todo : String -> Story
todo title =
    Story.Todo title


label : String -> Story
label title =
    Story.Label title


type alias Program =
    Main.Program


program : (String -> Cmd msg) -> List Story -> Program
program onSettingsChange stories =
    Main.run onSettingsChange stories
