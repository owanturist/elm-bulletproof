module Internal exposing (Addons, Knob(..), KnobsState, Story(..), StoryPayload, initialAddons)

import AVL.Dict as Dict exposing (Dict)


type alias Addons =
    { knobs : KnobsState
    }


initialAddons : Addons
initialAddons =
    Addons Dict.empty


type Story view
    = Story (StoryPayload view)


type alias StoryPayload view =
    { title : String
    , knobs : List ( String, Knob )
    , view : Addons -> view
    }



-- K N O B S


type Knob
    = KnobInt Int
    | KnobString String


type alias KnobsState =
    Dict String (Dict String Knob)
