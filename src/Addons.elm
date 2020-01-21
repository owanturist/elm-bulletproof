module Addons exposing (Addons, initial)

import Knob


type alias Addons =
    { knobs : Knob.State
    }


initial : Addons
initial =
    { knobs = Knob.initial
    }
