module Story exposing (Payload, Story(..))

import Addons exposing (Addons)
import Knob exposing (Knob)


type Story view
    = Story (Payload view)
    | Group String (List (Story view))
    | Empty


type alias Payload view =
    { title : String
    , knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }
