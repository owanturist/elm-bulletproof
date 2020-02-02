module Navigation exposing (Model, Msg, initial, open, update, view)

import AVL.Set as Set exposing (Set)
import Css
import Html.Styled as Html exposing (Html, a, div, span, styled, text)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Icon
import Palette
import Renderer exposing (Renderer(..))
import Router
import Story exposing (Story(..))
import Utils exposing (ifelse, onSpaceOrEnter)



-- M O D E L


type alias Model =
    Set Story.Path


initial : Model
initial =
    Set.empty


open : Story.Path -> Model -> Model
open path model =
    openHelp [] path model


openHelp : Story.Path -> Story.Path -> Model -> Model
openHelp prev path model =
    case path of
        [] ->
            model

        first :: rest ->
            let
                pathToOpen =
                    first :: prev
            in
            openHelp pathToOpen rest (Set.insert pathToOpen model)



-- U P D A T E


type Msg
    = GoToStory Story.Path
    | Toggle Story.Path


update : Router.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GoToStory storyPath ->
            ( model
            , Router.push key (Router.ToStory storyPath)
            )

        Toggle path ->
            ( Set.toggle path model
            , Cmd.none
            )



-- V I E W


toKey : Story.Path -> String
toKey =
    String.join "<-@->"


viewSpacer : Int -> Html msg
viewSpacer n =
    if n > 0 then
        styled span
            [ Css.display Css.inlineBlock
            , Css.width (Css.px (toFloat n * 22))
            ]
            []
            []

    else
        text ""


styledIconHolder : List (Html msg) -> Html msg
styledIconHolder =
    styled span
        [ Css.display Css.inlineBlock
        , Css.marginRight (Css.px 4)
        , Css.width (Css.px 18)
        , Css.verticalAlign Css.middle
        ]
        []


viewTodo : Story.Path -> String -> ( String, Html msg )
viewTodo path title =
    ( toKey ("TODO" :: title :: path)
    , div
        [ css (cssStaticItem False)
        , Attributes.tabindex -1
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder [ Icon.tools ]
        , text title
        ]
    )


styledLabel : List (Html msg) -> Html msg
styledLabel =
    styled div
        [ Css.marginTop (Css.px 8)
        , Css.padding4 (Css.px 4) (Css.px 12) (Css.px 5) (Css.px 8)
        , Css.color Palette.dark50
        , Css.fontSize (Css.px 12)
        , Css.fontWeight Css.bold
        , Css.letterSpacing (Css.em 0.25)
        , Css.firstChild
            [ Css.marginTop Css.zero
            ]
        ]
        []


viewLabel : Story.Path -> String -> ( String, Html msg )
viewLabel path title =
    ( toKey ("LABEL" :: title :: path)
    , styledLabel
        [ viewSpacer (List.length path)
        , text (String.toUpper title)
        ]
    )


cssStaticItem : Bool -> List Css.Style
cssStaticItem active =
    [ Css.display Css.block
    , Css.padding4 (Css.px 4) (Css.px 12) (Css.px 4) (Css.px 8)
    , Css.textDecoration Css.none
    , Css.outline Css.none
    , Css.color Css.inherit
    , if active then
        Css.batch
            [ Css.backgroundColor Palette.blue
            , Css.color Palette.white
            , Css.fontWeight Css.bold
            ]

      else
        Css.batch []
    ]


cssItem : Bool -> List Css.Style
cssItem active =
    Css.focus
        [ Css.backgroundColor (ifelse active Palette.blueDark Palette.smoke)
        ]
        :: Css.hover
            [ Css.backgroundColor (ifelse active Palette.blueDark Palette.smoke)
            ]
        :: cssStaticItem active


viewStoryLink : Bool -> Story.Path -> String -> ( String, Html Msg )
viewStoryLink active path title =
    let
        storyPath =
            title :: path

        exactStoryPath =
            List.reverse storyPath
    in
    ( toKey ("STORY" :: storyPath)
    , a
        [ css (cssItem active)
        , Attributes.rel "noopener noreferrer"
        , Attributes.tabindex 0
        , Attributes.title (String.join " / " exactStoryPath)
        , Router.ToStory exactStoryPath
            |> Router.toString
            |> Attributes.href
        , onSpaceOrEnter (GoToStory exactStoryPath)
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder [ Icon.elm ]
        , text title
        ]
    )


styledFolder : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledFolder active =
    styled div (Css.cursor Css.pointer :: cssItem active)


viewFolder : Model -> Story.Path -> Story.Path -> String -> List (Story error Renderer) -> List ( String, Html Msg )
viewFolder model current path title stories =
    let
        folderPath =
            title :: path

        opened =
            Set.member folderPath model

        ( active, nextCurrent ) =
            case current of
                [] ->
                    ( False, [] )

                fragmentID :: rest ->
                    if fragmentID == title then
                        ( not opened, rest )

                    else
                        ( False, [] )
    in
    ( toKey ("FOLDER" :: folderPath)
    , styledFolder active
        [ Attributes.attribute "role" "button"
        , Attributes.tabindex 0
        , Attributes.title (String.join " / " (List.reverse folderPath))
        , Events.onClick (Toggle folderPath)
        , onSpaceOrEnter (Toggle folderPath)
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder
            [ if opened then
                ifelse (List.isEmpty stories) Icon.folderEmptyOpen Icon.folderOpen

              else
                ifelse (List.isEmpty stories) Icon.folderEmpty Icon.folder
            ]
        , text title
        ]
    )
        :: List.concatMap (viewItem model nextCurrent folderPath) (ifelse opened stories [])


viewItem : Model -> Story.Path -> Story.Path -> Story error Renderer -> List ( String, Html Msg )
viewItem model current path story =
    case story of
        Story.Label title ->
            [ viewLabel path title
            ]

        Story.Todo title ->
            [ viewTodo path title
            ]

        Story.Single storyID _ ->
            case current of
                fragmentID :: [] ->
                    [ viewStoryLink (fragmentID == storyID) path storyID
                    ]

                _ ->
                    [ viewStoryLink False path storyID
                    ]

        Story.Batch folderID stories ->
            viewFolder model current path folderID stories

        Story.Fail _ _ ->
            []


cssContainer : List Css.Style
cssContainer =
    [ Css.display Css.table
    , Css.width (Css.pct 100)
    , Css.whiteSpace Css.noWrap
    , Css.property "user-select" "none"
    , Css.fontFamilies Palette.font
    , Css.fontSize (Css.px 13)
    , Css.color Palette.dark
    ]


view : Story.Path -> List (Story error Renderer) -> Model -> Html Msg
view current stories model =
    Keyed.node "div"
        [ css cssContainer
        ]
        (List.concatMap (viewItem model current []) stories)
