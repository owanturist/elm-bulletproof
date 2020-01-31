module Settings exposing
    ( Orientation(..)
    , Settings
    , decoder
    , default
    , encoder
    , minDockHeight
    , minDockWidth
    , minNavigationWidth
    , minStoryHeight
    , minStoryWidth
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Orientation
    = Horizontal
    | Vertical


orientationEncoder : Orientation -> Value
orientationEncoder orientation =
    case orientation of
        Horizontal ->
            Encode.string "h"

        Vertical ->
            Encode.string "v"


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.andThen
        (\str ->
            case str of
                "h" ->
                    Decode.succeed Horizontal

                "v" ->
                    Decode.succeed Vertical

                _ ->
                    Decode.fail ("Unknown Orientation '" ++ str ++ "'")
        )
        Decode.string


type alias Settings =
    { fullscreen : Bool
    , navigationVisible : Bool
    , navigationWidth : Int
    , dockVisible : Bool
    , dockWidth : Int
    , dockHeight : Int
    , dockOrientation : Orientation
    , addPaddings : Bool
    , darkBackground : Bool
    , showGrid : Bool
    }


default : Settings
default =
    { fullscreen = False
    , navigationVisible = True
    , navigationWidth = 200
    , dockVisible = True
    , dockWidth = 400
    , dockHeight = 300
    , dockOrientation = Horizontal
    , addPaddings = False
    , darkBackground = False
    , showGrid = False
    }


encoder : Settings -> Value
encoder settings =
    Encode.list identity
        [ Encode.bool settings.fullscreen
        , Encode.bool settings.navigationVisible
        , Encode.int settings.navigationWidth
        , Encode.bool settings.dockVisible
        , Encode.int settings.dockWidth
        , Encode.int settings.dockHeight
        , orientationEncoder settings.dockOrientation
        , Encode.bool settings.addPaddings
        , Encode.bool settings.darkBackground
        , Encode.bool settings.showGrid
        ]


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap val fn =
    Decode.andThen (Decode.map >> (|>) val) fn


decoder : Decoder Settings
decoder =
    Decode.succeed Settings
        |> andMap (Decode.index 0 Decode.bool)
        |> andMap (Decode.index 1 Decode.bool)
        |> andMap (Decode.index 2 Decode.int)
        |> andMap (Decode.index 3 Decode.bool)
        |> andMap (Decode.index 4 Decode.int)
        |> andMap (Decode.index 5 Decode.int)
        |> andMap (Decode.index 6 orientationDecoder)
        |> andMap (Decode.index 7 Decode.bool)
        |> andMap (Decode.index 8 Decode.bool)
        |> andMap (Decode.index 9 Decode.bool)


minNavigationWidth : Int
minNavigationWidth =
    100


minDockWidth : Int
minDockWidth =
    300


minDockHeight : Int
minDockHeight =
    200


minStoryWidth : Int
minStoryWidth =
    300


minStoryHeight : Int
minStoryHeight =
    200
