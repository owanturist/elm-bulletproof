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


hexColorFraction : Int -> String
hexColorFraction fraction =
    String.padLeft 2 '0' (Hex.toString fraction)


makeColor : Int -> Int -> Int -> Color
makeColor r g b =
    let
        hex =
            String.join "" ("#" :: List.map hexColorFraction [ r, g, b ])
    in
    Color hex r g b r g b


parse : Char -> Char -> Char -> Char -> Char -> Char -> Maybe Color
parse r1 r2 g1 g2 b1 b2 =
    Result.map3 makeColor
        (Hex.fromString (String.fromList [ r1, r2 ]))
        (Hex.fromString (String.fromList [ g1, g2 ]))
        (Hex.fromString (String.fromList [ b1, b2 ]))
        |> Result.toMaybe


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
