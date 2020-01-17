module Main exposing (main)

import Browser
import Counter


main : Program () Counter.Model Counter.Msg
main =
    Browser.sandbox
        { init = Counter.initial
        , update = Counter.update
        , view = Counter.view
        }
