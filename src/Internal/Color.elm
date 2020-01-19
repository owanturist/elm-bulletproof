module Internal.Color exposing (Color, fromString)

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


makeColor : String -> Int -> Int -> Int -> Color
makeColor hex r g b =
    Color hex r g b r g b


parse : Char -> Char -> Char -> Char -> Char -> Char -> Maybe Color
parse r1 r2 g1 g2 b1 b2 =
    Result.map3 (makeColor (String.fromList [ '#', r1, r2, g1, g2, b1, b2 ]))
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
