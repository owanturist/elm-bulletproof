module Palette exposing
    ( blue
    , blueDark
    , cloud
    , dark
    , dark05
    , font
    , gray
    , gray05
    , smoke
    , white
    )


joinComma : List Int -> String
joinComma =
    String.join "," << List.map String.fromInt


rgb : Int -> Int -> Int -> String
rgb r g b =
    "rgb(" ++ joinComma [ r, g, b ] ++ ")"


rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
    "rgba(" ++ joinComma [ r, g, b ] ++ "," ++ String.fromFloat a ++ ")"


blue : String
blue =
    rgb 30 165 253


blueDark : String
blueDark =
    rgb 8 155 251


cloud : String
cloud =
    rgb 246 249 252


dark : String
dark =
    rgb 51 51 51


dark05 : String
dark05 =
    rgba 51 51 51 0.5


gray : String
gray =
    rgb 153 153 153


gray05 : String
gray05 =
    rgba 153 153 153 0.5


smoke : String
smoke =
    rgba 0 0 0 0.05


white : String
white =
    rgb 255 255 255


font : String
font =
    [ "\"Nunito Sans\""
    , "-apple-system"
    , "\".SFNSText-Regular\""
    , "\"San Francisco\""
    , "BlinkMacSystemFont"
    , "\"Segoe UI\""
    , "\"Helvetica Neue\""
    , "Helvetica"
    , "Arial"
    , "sans-serif"
    ]
        |> String.join ","
