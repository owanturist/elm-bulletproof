module Navigation exposing (Model, Msg, css, initial, open, update, view)

import Array exposing (length)
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
    Style.sheet
        [ navigation__spacer
        , navigation__icon_box

        --
        , navigation__label
        , Style.selector ("* + ." ++ Style.classNameString navigation__label)
            [ Style.rule "margin-top" "8px"
            ]

        --
        , navigation__item
        , navigation__item_active
        , Style.each
            [ Style.hover navigation__item_active
            , Style.focusVisible navigation__item_active
            ]
            [ Style.rule "background" Palette.blueDark
            ]
        , navigation__item_interactive
        , Style.each
            [ Style.hover navigation__item_interactive
            , Style.focusVisible navigation__item_interactive
            ]
            [ Style.rule "background" Palette.smoke
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
        [ Style.rule "background" Palette.blue
        , Style.rule "color" Palette.white
        , Style.rule "font-weight" "bold"
        , Style.rule "cursor" "pointer"
        ]


navigation__header : Style.Selector
navigation__header =
    Style.class "navigation__header"
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
            [ Style.className navigation__spacer
            , Attributes.style "width" (px (n * 22))
            ]
            []

    else
        text ""


viewIconBox : Html msg -> Html msg
viewIconBox =
    span [ Style.className navigation__icon_box ] << List.singleton


viewTodo : Int -> String -> Html msg
viewTodo indent title =
    div
        [ Style.className navigation__item
        , Attributes.tabindex -1
        ]
        [ viewSpacer indent
        , viewIconBox Icon.tools
        , text title
        ]


viewLabel : Int -> String -> Html msg
viewLabel indent title =
    div
        [ Style.className navigation__label
        ]
        [ viewSpacer indent
        , text (String.toUpper title)
        ]


viewStoryLink : Bool -> String -> Int -> String -> String -> Html (Story.Path -> Msg)
viewStoryLink active title indent url tooltip =
    a
        [ Style.className navigation__item
        , Style.className (ifelse active navigation__item_active navigation__item_interactive)
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
        [ Style.className navigation__item
        , Style.className (ifelse active navigation__item_active navigation__item_interactive)
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


viewFolderTree : Story.Path -> Story.Path -> String -> Model -> Story (Html ()) -> List ( String, Html Msg )
viewFolderTree current path title =
    if List.head current == Just title then
        viewItem (List.drop 1 current) (title :: path)

    else
        viewItem [] (title :: path)


viewItem : Story.Path -> Story.Path -> Model -> Story (Html ()) -> List ( String, Html Msg )
viewItem current path model story =
    case story of
        Story.Label title ->
            [ ( toKey "LABEL" title
              , Lazy.lazy2 viewLabel (List.length path) title
              )
            ]

        Story.Todo title ->
            [ ( toKey "TODO" title
              , Lazy.lazy2 viewTodo (List.length path) title
              )
            ]

        Story.Single title _ ->
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
            [ ( toKey "STORY" title
              , Lazy.lazy5 viewStoryLink active title indent url tooltip
                    |> Html.map ((|>) storyPath)
              )
            ]

        Story.Folder title substory ->
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
            if Story.isEmpty substory then
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
                  , Keyed.node "div" [] (viewFolderTree current path title model substory)
                  )
                ]

            else
                [ ( toKey "FOLDER" title
                  , Lazy.lazy5 viewFolder False (List.head current == Just title) title indent tooltip
                        |> Html.map ((|>) folderPath)
                  )
                ]

        Story.Batch stories ->
            List.concatMap (viewItem current path model) stories


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
                (viewItem current [] model story)
            ]
        ]
