module Internal exposing (Addons, Story(..), StoryPayload, initialAddons)

import Knob exposing (Knob)


type alias Addons =
    { knobs : Knob.State
    }


initialAddons : Addons
initialAddons =
    { knobs = Knob.initial
    }


type Story view
    = Story (StoryPayload view)


type alias StoryPayload view =
    { title : String
    , knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }
