module Navigation exposing (Model, Msg, css, initial, open, update, view)

import Html exposing (Html, a, div, header, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Icon
import Palette
import Router
import Set exposing (Set)
import Story exposing (Story(..))
import Style
import Utils exposing (ifelse, onSpaceOrEnter, px)



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
    Style.batch
        [ Style.elements
            [ navigation__spacer
            , navigation__icon_box
            , navigation__label
            , navigation__item
            , navigation__item__active
            , navigation__item__interactive
            , navigation__header
            , navigation__container
            , navigation__scroller
            , navigation__root
            ]

        --
        , Style.selector ("* + ." ++ Style.className navigation__label)
            [ Style.rule "margin-top" "8px"
            ]
        ]


navigation__spacer : Style.Element
navigation__spacer =
    Style.el "navigation__spacer"
        [ Style.rule "display" "inline-block"
        ]


navigation__icon_box : Style.Element
navigation__icon_box =
    Style.el "navigation__icon_box"
        [ Style.rule "display" "inline-block"
        , Style.rule "margin-right" "4px"
        , Style.rule "width" "18px"
        , Style.rule "vertical-align" "middle"
        ]


navigation__label : Style.Element
navigation__label =
    Style.el "navigation__label"
        [ Style.rule "padding" "4px 12px 5px 8px"
        , Style.rule "color" Palette.dark05
        , Style.rule "font-weight" "bold"
        , Style.rule "font-size" "12px"
        , Style.rule "letter-spacing" "0.25em"
        ]


navigation__item : Style.Element
navigation__item =
    Style.el "navigation__item"
        [ Style.rule "display" "block"
        , Style.rule "padding" "4px 12px 4px 8px"
        , Style.rule "text-decoration" "none"
        , Style.rule "outline" "none"
        , Style.rule "color" "inherit"
        ]


navigation__item__interactive : Style.Element
navigation__item__interactive =
    [ Style.rule "cursor" "pointer"
    ]
        |> Style.mod navigation__item "interactive"
        |> Style.hover
            [ Style.rule "background" Palette.smoke
            ]
        |> Style.focusVisible
            [ Style.rule "background" Palette.smoke
            ]


navigation__item__active : Style.Element
navigation__item__active =
    [ Style.rule "background" Palette.blue
    , Style.rule "color" Palette.white
    , Style.rule "font-weight" "bold"
    , Style.rule "cursor" "pointer"
    ]
        |> Style.mod navigation__item "active"
        |> Style.hover
            [ Style.rule "background" Palette.blueDark
            ]
        |> Style.focusVisible
            [ Style.rule "background" Palette.blueDark
            ]


navigation__header : Style.Element
navigation__header =
    Style.el "navigation__header"
        [ Style.rule "flex" "0 0 auto"
        , Style.rule "padding" "16px 12px 16px 48px"
        , Style.rule "background" Palette.white
        , Style.rule "font-weight" "bold"
        , Style.rule "font-size" "16px"
        , Style.rule "line-height" "1"
        , Style.rule "letter-spacing" "0.05em"
        , Style.rule "box-shadow" ("0 0 10px " ++ Palette.smoke)
        , Style.rule "overflow" "hidden"
        ]


navigation__container : Style.Element
navigation__container =
    Style.el "navigation__container"
        [ Style.rule "flex" "1 0 0"
        , Style.rule "padding" "8px 0 20px"
        ]


navigation__scroller : Style.Element
navigation__scroller =
    Style.el "navigation__scroller"
        [ Style.rule "display" "flex"
        , Style.rule "width" "100%"
        , Style.rule "overflow" "auto"
        ]


navigation__root : Style.Element
navigation__root =
    Style.el "navigation__root"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "flex"
        , Style.rule "flex-direction" "column"
        , Style.rule "width" "100%"
        , Style.rule "height" "100%"
        , Style.rule "white-space" "nowrap"
        , Style.rule "user-select" "none"
        , Style.rule "color" Palette.dark
        , Style.rule "background" Palette.cloud
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font
        ]


toKey : String -> String -> String
toKey left right =
    left ++ "|" ++ right


stringifyPath : Story.Path -> String
stringifyPath =
    String.join " / "


viewSpacer : Int -> Html msg
viewSpacer n =
    if n > 0 then
        span
            [ Style.class navigation__spacer
            , Attributes.style "width" (px (n * 22))
            ]
            []

    else
        text ""


viewIconBox : Html msg -> Html msg
viewIconBox =
    span [ Style.class navigation__icon_box ] << List.singleton


viewLabel : String -> Int -> Html msg
viewLabel title indent =
    div
        [ Style.class navigation__label
        ]
        [ viewSpacer indent
        , text (String.toUpper title)
        ]


viewLabelItem : String -> Story.Path -> ( String, Html msg )
viewLabelItem title path =
    ( toKey "LABEL" title
    , Lazy.lazy2 viewLabel title (List.length path)
    )


viewTodo : String -> Int -> Html msg
viewTodo title indent =
    div
        [ Style.class navigation__item
        , Attributes.tabindex -1
        ]
        [ viewSpacer indent
        , viewIconBox Icon.tools
        , text title
        ]


viewTodoItem : String -> Story.Path -> ( String, Html msg )
viewTodoItem title path =
    ( toKey "STORY" title
    , Lazy.lazy2 viewTodo title (List.length path)
    )


viewStory : Bool -> String -> Int -> String -> String -> Html (Story.Path -> Msg)
viewStory active title indent url tooltip =
    a
        [ Style.class navigation__item
        , Style.class (ifelse active navigation__item__active navigation__item__interactive)
        , Attributes.rel "noopener noreferrer"
        , Attributes.tabindex 0
        , Attributes.title tooltip
        , Attributes.href url
        , onSpaceOrEnter GoToStory
        ]
        [ viewSpacer indent
        , viewIconBox Icon.elm
        , text title
        ]


viewStoryItem : String -> Story.Path -> Story.Path -> ( String, Html Msg )
viewStoryItem title path current =
    let
        storyPath =
            List.reverse (title :: path)

        active =
            current == [ title ]

        url =
            Router.toString storyPath

        indent =
            List.length path

        tooltip =
            stringifyPath storyPath
    in
    ( toKey "STORY" title
    , Lazy.lazy5 viewStory active title indent url tooltip
        |> Html.map ((|>) storyPath)
    )


viewMakeFolder :
    { title : String
    , indent : Int
    , active : Bool
    , tooltip : String
    , icon : Html msg
    , onClick : msg
    }
    -> Html msg
viewMakeFolder { title, indent, active, tooltip, icon, onClick } =
    div
        [ Style.class navigation__item
        , Style.class (ifelse active navigation__item__active navigation__item__interactive)
        , Attributes.attribute "role" "button"
        , Attributes.tabindex 0
        , Attributes.title tooltip
        , Events.onClick onClick
        , onSpaceOrEnter onClick
        ]
        [ viewSpacer indent
        , viewIconBox icon
        , text title
        ]


viewEmptyFolder : Bool -> String -> Int -> String -> Html (Story.Path -> Msg)
viewEmptyFolder opened title indent tooltip =
    viewMakeFolder
        { title = title
        , indent = indent
        , active = False
        , tooltip = tooltip
        , icon = ifelse opened Icon.folderEmptyOpen Icon.folderEmpty
        , onClick = Toggle
        }


viewFolder : Bool -> Bool -> String -> Int -> String -> Html (Story.Path -> Msg)
viewFolder opened active title indent tooltip =
    viewMakeFolder
        { title = title
        , indent = indent
        , active = active
        , tooltip = tooltip
        , icon = ifelse opened Icon.folderOpen Icon.folder
        , onClick = Toggle
        }


viewFolderTree : String -> Story.Path -> Story.Path -> Model -> Story view -> List ( String, Html Msg )
viewFolderTree title path current =
    let
        nextCurrent =
            if List.head current == Just title then
                List.drop 1 current

            else
                []
    in
    viewItem (title :: path) nextCurrent


viewFolderItem : String -> Story.Path -> Story.Path -> Model -> Story view -> List ( String, Html Msg )
viewFolderItem title path current model story =
    let
        folderPath =
            title :: path

        opened =
            Set.member folderPath model

        indent =
            List.length path

        tooltip =
            stringifyPath (List.reverse folderPath)
    in
    if Story.isEmpty story then
        [ ( toKey "FOLDER" title
          , Lazy.lazy4 viewEmptyFolder opened title indent tooltip
                |> Html.map ((|>) folderPath)
          )
        ]

    else if opened then
        [ ( toKey "FOLDER" title
          , Lazy.lazy5 viewFolder True False title indent tooltip
                |> Html.map ((|>) folderPath)
          )
        , ( toKey "FOLDER_TREE" title
          , Keyed.node "div" [] (viewFolderTree title path current model story)
          )
        ]

    else
        [ ( toKey "FOLDER" title
          , Lazy.lazy5 viewFolder False (List.head current == Just title) title indent tooltip
                |> Html.map ((|>) folderPath)
          )
        ]


viewItem : Story.Path -> Story.Path -> Model -> Story view -> List ( String, Html Msg )
viewItem path current model story =
    case story of
        Story.Label title ->
            [ viewLabelItem title path ]

        Story.Todo title ->
            [ viewTodoItem title path ]

        Story.Single title _ ->
            [ viewStoryItem title path current ]

        Story.Folder title substory ->
            viewFolderItem title path current model substory

        Story.Batch stories ->
            List.concatMap (viewItem path current model) stories


view : Story.Path -> Story view -> Model -> Html Msg
view current story model =
    div
        [ Style.class navigation__root
        ]
        [ header
            [ Style.class navigation__header
            ]
            [ text "BULLETPROOF"
            ]
        , div
            [ Style.class navigation__scroller
            ]
            [ Keyed.node "div"
                [ Style.class navigation__container
                ]
                (viewItem [] current model story)
            ]
        ]
