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
import Utils exposing (ifelse)



-- M O D E L


type alias Model =
    Set (List String)


initial : Model
initial =
    Set.empty


open : List String -> Model -> Model
open path model =
    openHelp [] path model


openHelp : List String -> List String -> Model -> Model
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
    = Toggle (List String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle path ->
            Set.toggle path model



-- V I E W


toKey : List String -> String
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


styledLabel : List (Html msg) -> Html msg
styledLabel =
    styled div
        [ Css.padding3 (Css.px 4) (Css.px 8) (Css.px 5)
        , Css.color Palette.dark50
        , Css.fontSize (Css.px 12)
        , Css.fontWeight Css.bold
        , Css.letterSpacing (Css.em 0.25)
        ]
        []


viewLabel : Int -> String -> ( String, Html msg )
viewLabel nesting title =
    ( toKey [ "LABEL", title ]
    , styledLabel
        [ viewSpacer nesting
        , text (String.toUpper title)
        ]
    )


cssStoryLink : Bool -> List Css.Style
cssStoryLink active =
    [ Css.display Css.block
    , Css.padding2 (Css.px 4) (Css.px 8)
    , Css.textDecoration Css.none
    , if active then
        Css.batch
            [ Css.backgroundColor Palette.blue
            , Css.color Palette.white
            , Css.fontWeight Css.bold
            ]

      else
        Css.batch
            [ Css.color Css.inherit
            , Css.hover
                [ Css.backgroundColor Palette.fog
                ]
            ]
    ]


viewStoryLink : Bool -> List String -> String -> ( String, Html msg )
viewStoryLink active path title =
    let
        storyPath =
            title :: path
    in
    ( toKey ("STORY" :: storyPath)
    , ifelse active
        span
        a
        [ css (cssStoryLink active)
        , Attributes.rel "noopener noreferrer"
        , storyPath
            |> List.reverse
            |> Router.ToStory
            |> Router.toString
            |> Attributes.href
        ]
        [ viewSpacer (List.length path)
        , styledIconHolder [ Icon.elm ]
        , text title
        ]
    )


styledFolder : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledFolder active =
    styled div
        [ Css.display Css.block
        , Css.padding2 (Css.px 4) (Css.px 8)
        , Css.outline Css.none
        , Css.cursor Css.pointer
        , if active then
            Css.batch
                [ Css.backgroundColor Palette.blue
                , Css.color Palette.white
                ]

          else
            Css.hover
                [ Css.backgroundColor Palette.fog
                ]
        ]


viewFolder : Model -> List String -> List String -> String -> List (Story Renderer) -> List ( String, Html Msg )
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
        , Events.onClick (Toggle folderPath)
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


viewItem : Model -> List String -> List String -> Story Renderer -> List ( String, Html Msg )
viewItem model current path story =
    case story of
        Story.Label title ->
            [ viewLabel (List.length path) title
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


cssContainer : List Css.Style
cssContainer =
    [ Css.width (Css.pct 100)
    , Css.property "user-select" "none"
    , Css.fontFamilies Palette.font
    , Css.fontSize (Css.px 13)
    , Css.color Palette.dark
    ]


view : List String -> List (Story Renderer) -> Model -> Html Msg
view current stories model =
    Keyed.node "div"
        [ css cssContainer
        ]
        (List.concatMap (viewItem model current []) stories)
