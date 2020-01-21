module Story exposing (Payload, Story(..))

import Addons exposing (Addons)
import Knob exposing (Knob)


type Story view
    = Story (Payload view)


type alias Payload view =
    { title : String
    , knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }
