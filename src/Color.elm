module Color exposing (Color, fromString, makeColor)

import Hex


type alias Color =
    { hex : String
    , red : Int
    , green : Int
    , blue : Int
    , r : Int
    , g : Int
    , b : Int
    }


colorFractionToHex : Int -> String
colorFractionToHex fraction =
    String.padLeft 2 '0' (Hex.toString fraction)


makeColor : Int -> Int -> Int -> Color
makeColor r g b =
    let
        hex =
            String.join "" ("#" :: List.map colorFractionToHex [ r, g, b ])
    in
    Color hex r g b r g b


colorFractionFromHex : Char -> Char -> Maybe Int
colorFractionFromHex hex1 hex2 =
    [ hex1, hex2 ]
        |> String.fromList
        |> Hex.fromString
        |> Result.toMaybe


parse : Char -> Char -> Char -> Char -> Char -> Char -> Maybe Color
parse r1 r2 g1 g2 b1 b2 =
    Maybe.map3 makeColor
        (colorFractionFromHex r1 r2)
        (colorFractionFromHex g1 g2)
        (colorFractionFromHex b1 b2)


dropHash : String -> String
dropHash str =
    if String.startsWith "#" str then
        String.dropLeft 1 str

    else
        str


fromString : String -> Maybe Color
fromString str =
    case String.toList (String.toLower (dropHash str)) of
        [ r, g, b ] ->
            parse r r g g b b

        [ r1, r2, g1, g2, b1, b2 ] ->
            parse r1 r2 g1 g2 b1 b2

        _ ->
            Nothing
