module Palette exposing (black, blue, cloud, dark, fog, font, white)

import Css


blue : Css.Color
blue =
    Css.hex "#1ea7fd"


cloud : Css.Color
cloud =
    Css.hex "#f6f9fc"


dark : Css.Color
dark =
    Css.hex "#333"


fog : Css.Color
fog =
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
