module Story.Range exposing (story)

import Bulletproof
import Bulletproof.Knob
import Range exposing (range)


story : Bulletproof.Story
story =
    Bulletproof.storyOf "Range"
        (\min max step value ->
            range (always ())
                "int-range"
                String.fromInt
                { min = min
                , max = max
                , step = step
                , value = value
                }
                |> Bulletproof.html
        )
        |> Bulletproof.Knob.int "Min" 10 []
        |> Bulletproof.Knob.int "Max" 50 []
        |> Bulletproof.Knob.int "Step" 1 []
        |> Bulletproof.Knob.int "Value" 20 []
