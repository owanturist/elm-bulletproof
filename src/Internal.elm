module Internal exposing (Addons, Story(..), StoryPayload, initialAddons)

import Internal.Knob exposing (Knob)


type alias Addons =
    { knobs : Internal.Knob.State
    }


initialAddons : Addons
initialAddons =
    { knobs = Internal.Knob.initial
    }


type Story view
    = Story (StoryPayload view)


type alias StoryPayload view =
    { title : String
    , knobs : List ( String, Knob )
    , view : Addons -> view
    }
