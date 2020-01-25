module Palette exposing
    ( black
    , blue
    , blueDark
    , cloud
    , dark
    , dark50
    , font
    , gray
    , gray50
    , smoke
    , white
    )

import Css


blue : Css.Color
blue =
    Css.rgb 30 165 253


blueDark : Css.Color
blueDark =
    Css.rgb 8 155 251


cloud : Css.Color
cloud =
    Css.hex "#f6f9fc"


dark : Css.Color
dark =
    Css.rgb 51 51 51


dark50 : Css.Color
dark50 =
    Css.rgba 51 51 51 0.5


gray : Css.Color
gray =
    Css.rgb 153 153 153


gray50 : Css.Color
gray50 =
    Css.rgba 153 153 153 0.5


smoke : Css.Color
smoke =
    Css.rgba 0 0 0 0.05


black : Css.Color
black =
    Css.hex "#000"


white : Css.Color
white =
    Css.hex "#fff"


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
