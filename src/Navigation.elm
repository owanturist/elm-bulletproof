module Navigation exposing (Model, Msg, initial, open, update, view)

import AVL.Set as Set exposing (Set)
import Css
import Html.Styled as Html exposing (Html, a, div, span, styled, text)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
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

        _ :: [] ->
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
    String.join ""


styledStoryLink : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledStoryLink active =
    styled a
        [ Css.display Css.block
        , Css.padding2 (Css.px 4) (Css.px 8)
        , if active then
            Css.backgroundColor Palette.aqua

          else
            Css.backgroundColor Css.transparent
        ]


viewSpacer : Int -> Html msg
viewSpacer n =
    if n > 0 then
        styled span
            [ Css.display Css.inlineBlock
            , Css.width (Css.px (toFloat n * 8))
            ]
            []
            []

    else
        text ""


viewLink : Bool -> List String -> String -> ( String, Html msg )
viewLink active path title =
    let
        storyPath =
            title :: path
    in
    ( toKey storyPath
    , styledStoryLink active
        [ Attributes.rel "noopener noreferrer"
        , storyPath
            |> List.reverse
            |> Router.ToStory
            |> Router.toString
            |> Attributes.href
        ]
        [ viewSpacer (List.length path)
        , text "* "
        , text title
        ]
    )


styledFolder : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledFolder active =
    styled div
        [ Css.display Css.block
        , if active then
            Css.backgroundColor Palette.aqua

          else
            Css.backgroundColor Css.transparent
        , Css.cursor Css.pointer
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
    ( toKey folderPath
    , styledFolder active
        [ Attributes.attribute "role" "button"
        , Attributes.tabindex 0
        , Events.onClick (Toggle folderPath)
        ]
        [ viewSpacer (List.length path)
        , text (ifelse opened "V" ">")
        , text title
        ]
    )
        :: List.concatMap (viewItem model nextCurrent folderPath) (ifelse opened stories [])


viewItem : Model -> List String -> List String -> Story Renderer -> List ( String, Html Msg )
viewItem model current path story =
    case ( current, story ) of
        ( fragmentID :: [], Story.Single storyID _ ) ->
            [ viewLink (fragmentID == storyID) path storyID
            ]

        ( _, Story.Single storyID _ ) ->
            [ viewLink False path storyID
            ]

        ( _, Story.Batch folderID stories ) ->
            viewFolder model current path folderID stories


cssScroller : List Css.Style
cssScroller =
    [ Css.width (Css.pct 100)
    , Css.overflow Css.auto
    ]


view : List String -> List (Story Renderer) -> Model -> Html Msg
view current stories model =
    Keyed.node "div"
        [ css cssScroller
        ]
        (List.concatMap (viewItem model current []) stories)
