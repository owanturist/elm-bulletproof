module Navigation exposing (Model, Msg, initial, open, update, view)

import Css
import Html.Styled as Html exposing (Html, a, div, header, span, styled, text)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Icon
import Palette
import Router
import Set exposing (Set)
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
            , Router.push key storyPath
            )

        Toggle path ->
            ( if Set.member path model then
                Set.remove path model

              else
                Set.insert path model
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


viewTodo : Story.Path -> String -> Html msg
viewTodo path title =
    div
        [ css (cssStaticItem False)
        , Attributes.tabindex -1
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder [ Icon.tools ]
        , text title
        ]


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


viewLabel : Story.Path -> String -> Html msg
viewLabel path title =
    styledLabel
        [ viewSpacer (List.length path)
        , text (String.toUpper title)
        ]


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


viewStoryLink : Bool -> Story.Path -> String -> Html Msg
viewStoryLink active path title =
    let
        storyPath =
            title :: path

        exactStoryPath =
            List.reverse storyPath
    in
    a
        [ css (cssItem active)
        , Attributes.rel "noopener noreferrer"
        , Attributes.tabindex 0
        , Attributes.title (String.join " / " exactStoryPath)
        , Attributes.href (Router.toString exactStoryPath)
        , onSpaceOrEnter (GoToStory exactStoryPath)
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder [ Icon.elm ]
        , text title
        ]


styledFolder : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledFolder active =
    styled div (Css.cursor Css.pointer :: cssItem active)


viewFolderTree : Model -> Story.Path -> Story.Path -> String -> List (Story (Html ())) -> List ( String, Html Msg )
viewFolderTree model current path title stories =
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


viewItem : Model -> Story.Path -> Story.Path -> Story (Html ()) -> List ( String, Html Msg )
viewItem model current path story =
    case story of
        Story.Label title ->
            [ ( toKey ("LABEL" :: title :: path)
              , viewLabel path title
              )
            ]

        Story.Todo title ->
            [ ( toKey ("TODO" :: title :: path)
              , viewTodo path title
              )
            ]

        Story.Single title _ ->
            [ ( toKey ("STORY" :: title :: path)
              , viewStoryLink (current == [ title ]) path title
              )
            ]

        Story.Folder title stories ->
            viewFolderTree model current path title stories


styledHeader : List (Html msg) -> Html msg
styledHeader =
    styled header
        [ Css.position Css.absolute
        , Css.top Css.zero
        , Css.right Css.zero
        , Css.left Css.zero
        , Css.padding4 (Css.px 16) (Css.px 12) (Css.px 16) (Css.px 48)
        , Css.backgroundColor Palette.white
        , Css.fontWeight Css.bold
        , Css.fontSize (Css.px 16)
        , Css.lineHeight (Css.int 1)
        , Css.letterSpacing (Css.em 0.05)
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        ]
        []


cssContainer : List Css.Style
cssContainer =
    [ Css.flex3 (Css.int 1) Css.zero Css.zero
    , Css.padding3 (Css.px 8) Css.zero (Css.px 20)
    ]


styledScroller : List (Html msg) -> Html msg
styledScroller =
    styled div
        [ Css.displayFlex
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.overflow Css.auto
        ]
        []


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.position Css.relative
        , Css.paddingTop (Css.px 48)
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.whiteSpace Css.noWrap
        , Css.property "user-select" "none"
        , Css.backgroundColor Palette.cloud
        , Css.color Palette.dark
        , Css.fontFamilies Palette.font
        , Css.fontSize (Css.px 13)
        ]
        []


view : Story.Path -> List (Story (Html ())) -> Model -> Html Msg
view current stories model =
    styledRoot
        [ styledHeader
            [ text "BULLETPROOF"
            ]
        , styledScroller
            [ Keyed.node "div"
                [ css cssContainer
                ]
                (List.concatMap (viewItem model current []) stories)
            ]
        ]
