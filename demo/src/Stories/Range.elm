module Stories.Range exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html.Styled
import Range exposing (range)


story : Bulletproof.Story
story =
    Bulletproof.folder "Range"
        [ Bulletproof.story "Int"
            (\min max step value ->
                range (always ())
                    "int-range"
                    String.fromInt
                    { min = min
                    , max = max
                    , step = step
                    , value = value
                    }
            )
            |> Bulletproof.Knob.int "Min" 10 []
            |> Bulletproof.Knob.int "Max" 20 []
            |> Bulletproof.Knob.int "Step" 1 []
            |> Bulletproof.Knob.int "Value" 15 []

        --
        , Bulletproof.story "Float"
            (\min max step value ->
                range (always ())
                    "float-range"
                    String.fromFloat
                    { min = min
                    , max = max
                    , step = step
                    , value = value
                    }
            )
            |> Bulletproof.Knob.float "Min" 1 []
            |> Bulletproof.Knob.float "Max" 5 []
            |> Bulletproof.Knob.float "Step" 0.05 []
            |> Bulletproof.Knob.float "Value" 3 []
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
