module Navigation exposing (Model, Msg, css, initial, open, update, view)

import Html.Styled as Html exposing (Html, a, div, header, span, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Icon
import Palette
import Router
import Set exposing (Set)
import Story exposing (Story(..))
import Style
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


css : Style.Sheet
css =
    Style.sheet
        [ navigation__spacer
        , navigation__icon_box

        --
        , navigation__label
        , Style.adjacent "*"
            navigation__label
            [ Style.rule "margin-top" "8px"
            ]

        --
        , navigation__item
        , navigation__item_active
        , Style.all
            [ Style.hover navigation__item_active
            , Style.focusVisible navigation__item_active
            ]
            [ Style.rule "background" Palette.blueDark_
            ]
        , navigation__item_interactive
        , Style.all
            [ Style.hover navigation__item_interactive
            , Style.focusVisible navigation__item_interactive
            ]
            [ Style.rule "background" Palette.smoke_
            ]

        --
        , navigation__header
        , navigation__container
        , navigation__scroller
        , navigation__root
        ]


navigation__spacer : Style.Selector
navigation__spacer =
    Style.class "navigation__spacer"
        [ Style.rule "display" "inline-block"
        ]


navigation__icon_box : Style.Selector
navigation__icon_box =
    Style.class "navigation__icon_box"
        [ Style.rule "display" "inline-block"
        , Style.rule "margin-right" "4px"
        , Style.rule "width" "18px"
        , Style.rule "vertical-align" "middle"
        ]


navigation__label : Style.Selector
navigation__label =
    Style.class "navigation__label"
        [ Style.rule "padding" "4px 12px 5px 8px"
        , Style.rule "color" Palette.dark05
        , Style.rule "font-weight" "bold"
        , Style.rule "font-size" "12px"
        , Style.rule "letter-spacing" "0.25em"
        ]


navigation__item : Style.Selector
navigation__item =
    Style.class "navigation__item"
        [ Style.rule "display" "block"
        , Style.rule "padding" "4px 12px 4px 8px"
        , Style.rule "text-decoration" "none"
        , Style.rule "outline" "none"
        , Style.rule "color" "inherit"
        ]


navigation__item_interactive : Style.Selector
navigation__item_interactive =
    Style.class "navigation__item_interactive"
        [ Style.rule "cursor" "pointer"
        ]


navigation__item_active : Style.Selector
navigation__item_active =
    Style.class "navigation__item_active"
        [ Style.rule "background" Palette.blue_
        , Style.rule "color" Palette.white_
        , Style.rule "font-weight" "bold"
        , Style.rule "cursor" "pointer"
        ]


navigation__header : Style.Selector
navigation__header =
    Style.class "navigation__header"
        [ Style.rule "flex" "0 0 auto"
        , Style.rule "padding" "16px 12px 16px 48px"
        , Style.rule "background" Palette.white_
        , Style.rule "font-weight" "bold"
        , Style.rule "font-size" "16px"
        , Style.rule "line-height" "1"
        , Style.rule "letter-spacing" "0.05em"
        , Style.rule "box-shadow" ("0 0 10px " ++ Palette.smoke_)
        , Style.rule "overflow" "hidden"
        ]


navigation__container : Style.Selector
navigation__container =
    Style.class "navigation__container"
        [ Style.rule "flex" "1 0 0"
        , Style.rule "padding" "8px 0 20px"
        ]


navigation__scroller : Style.Selector
navigation__scroller =
    Style.class "navigation__scroller"
        [ Style.rule "display" "flex"
        , Style.rule "width" "100%"
        , Style.rule "overflow" "auto"
        ]


navigation__root : Style.Selector
navigation__root =
    Style.class "navigation__root"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "flex"
        , Style.rule "flex-direction" "column"
        , Style.rule "width" "100%"
        , Style.rule "height" "100%"
        , Style.rule "white-space" "nowrap"
        , Style.rule "user-select" "none"
        , Style.rule "color" Palette.dark_
        , Style.rule "background" Palette.cloud_
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font_
        ]


toKey : Story.Path -> String
toKey =
    String.join ""


viewSpacer : Int -> Html msg
viewSpacer n =
    if n > 0 then
        span
            [ Style.className navigation__spacer
            , Attributes.style "width" (String.fromInt (n * 22) ++ "px")
            ]
            []

    else
        text ""


viewIconBox : List (Html msg) -> Html msg
viewIconBox =
    span [ Style.className navigation__icon_box ]


viewTodo : Story.Path -> String -> Html msg
viewTodo path title =
    div
        [ Style.className navigation__item
        , Attributes.tabindex -1
        ]
        [ viewSpacer (List.length path)
        , viewIconBox [ Icon.tools ]
        , text title
        ]


viewLabel : Story.Path -> String -> Html msg
viewLabel path title =
    div
        [ Style.className navigation__label
        ]
        [ viewSpacer (List.length path)
        , text (String.toUpper title)
        ]


viewStoryLink : Bool -> Story.Path -> String -> Html Msg
viewStoryLink active path title =
    let
        storyPath =
            title :: path

        exactStoryPath =
            List.reverse storyPath
    in
    a
        [ Style.className navigation__item
        , Style.className (ifelse active navigation__item_active navigation__item_interactive)
        , Attributes.rel "noopener noreferrer"
        , Attributes.tabindex 0
        , Attributes.title (String.join " / " exactStoryPath)
        , Attributes.href (Router.toString exactStoryPath)
        , onSpaceOrEnter (GoToStory exactStoryPath)
        ]
        [ viewSpacer (List.length path)
        , viewIconBox [ Icon.elm ]
        , text title
        ]


viewFolderTree : Model -> Story.Path -> Story.Path -> String -> Story (Html ()) -> List ( String, Html Msg )
viewFolderTree model current path title story =
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

        stories =
            if opened then
                viewItem model nextCurrent folderPath story

            else
                []
    in
    ( toKey ("FOLDER" :: folderPath)
    , div
        [ Style.className navigation__item
        , Style.className (ifelse active navigation__item_active navigation__item_interactive)
        , Attributes.attribute "role" "button"
        , Attributes.tabindex 0
        , Attributes.title (String.join " / " (List.reverse folderPath))
        , Events.onClick (Toggle folderPath)
        , onSpaceOrEnter (Toggle folderPath)
        ]
        [ viewSpacer (List.length path)
        , viewIconBox
            [ if opened then
                ifelse (Story.isEmpty story) Icon.folderEmptyOpen Icon.folderOpen

              else
                ifelse (Story.isEmpty story) Icon.folderEmpty Icon.folder
            ]
        , text title
        ]
    )
        :: stories


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

        Story.Folder title substory ->
            viewFolderTree model current path title substory

        Story.Batch stories ->
            List.concatMap (viewItem model current path) stories


view : Story.Path -> Story (Html ()) -> Model -> Html Msg
view current story model =
    div
        [ Style.className navigation__root
        ]
        [ header
            [ Style.className navigation__header
            ]
            [ text "BULLETPROOF"
            ]
        , div
            [ Style.className navigation__scroller
            ]
            [ Keyed.node "div"
                [ Style.className navigation__container
                ]
                (viewItem model current [] story)
            ]
        ]
