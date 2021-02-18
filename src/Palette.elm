module Palette exposing
    ( black
    , blue
    , blueDark
    , cloud
    , cloud_
    , dark
    , dark50
    , dark_
    , font
    , font_
    , gray
    , gray05
    , gray50
    , gray_
    , smoke
    , smoke_
    , transparent
    , white
    , white_
    )

import Css


joinComma : List Int -> String
joinComma =
    String.join "," << List.map String.fromInt


rgb : Int -> Int -> Int -> String
rgb r g b =
    "rgb(" ++ joinComma [ r, g, b ] ++ ")"


rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
    "rgba(" ++ joinComma [ r, g, b ] ++ "," ++ String.fromFloat a ++ ")"


transparent : Css.Color
transparent =
    Css.rgba 0 0 0 0


blue : Css.Color
blue =
    Css.rgb 30 165 253


blueDark : Css.Color
blueDark =
    Css.rgb 8 155 251


cloud : Css.Color
cloud =
    Css.hex "#f6f9fc"


cloud_ : String
cloud_ =
    rgb 246 249 252


dark : Css.Color
dark =
    Css.rgb 51 51 51


dark_ : String
dark_ =
    rgb 51 51 51


dark50 : Css.Color
dark50 =
    Css.rgba 51 51 51 0.5


gray : Css.Color
gray =
    Css.rgb 153 153 153


gray_ : String
gray_ =
    rgb 153 153 153


gray50 : Css.Color
gray50 =
    Css.rgba 153 153 153 0.5


gray05 : String
gray05 =
    rgba 153 153 153 0.5


smoke : Css.Color
smoke =
    Css.rgba 0 0 0 0.05


smoke_ : String
smoke_ =
    rgba 0 0 0 0.05


black : Css.Color
black =
    Css.hex "#000"


white : Css.Color
white =
    Css.hex "#fff"


white_ : String
white_ =
    rgb 255 255 255


font : List String
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


font_ : String
font_ =
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
