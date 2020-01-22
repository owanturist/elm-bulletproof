module Story exposing (Payload, Story(..))

import Addons exposing (Addons)
import Knob exposing (Knob)
import Path exposing (Path)


type Story view
    = Story Path (Payload view)
    | Group String (List (Story view))
    | Empty


type alias Payload view =
    { knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }
