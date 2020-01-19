module Internal.Color exposing (Color, black, decoder, fromString)

import Hex
import Json.Decode as Decode exposing (Decoder)


type alias Color =
    { hex : String
    , red : Int
    , green : Int
    , blue : Int
    , r : Int
    , g : Int
    , b : Int
    }


black : Color
black =
    makeColor "#000" 0 0 0


makeColor : String -> Int -> Int -> Int -> Color
makeColor hex r g b =
    Color hex r g b r g b


parse : Char -> Char -> Char -> Char -> Char -> Char -> Result String Color
parse r1 r2 g1 g2 b1 b2 =
    Result.map3 (makeColor (String.fromList [ '#', r1, r2, g1, g2, b1, b2 ]))
        (Hex.fromString (String.fromList [ r1, r2 ]))
        (Hex.fromString (String.fromList [ g1, g2 ]))
        (Hex.fromString (String.fromList [ b1, b2 ]))


dropHash : String -> String
dropHash str =
    if String.startsWith "#" str then
        String.dropLeft 1 str

    else
        str


fromString : String -> Result String Color
fromString str =
    case String.toList (String.toLower (dropHash str)) of
        [ r, g, b ] ->
            parse r r g g b b

        [ r1, r2, g1, g2, b1, b2 ] ->
            parse r1 r2 g1 g2 b1 b2

        rest ->
            if List.length rest < 3 then
                Err "Hex color string is too short. Expects at least 3 components."

            else
                Err "Hex color string is too long. Expects no more than 8 components."


decoder : Decoder Color
decoder =
    Decode.andThen
        (\str ->
            case fromString str of
                Err error ->
                    Decode.fail error

                Ok color ->
                    Decode.succeed color
        )
        Decode.string
