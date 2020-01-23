module Story exposing (Payload, Story(..))

import Addons exposing (Addons)
import Knob exposing (Knob)
import Renderer exposing (Renderer)


type Story view
    = None
    | Single String (Payload view)
    | Batch String (List (Story Renderer))


type alias Payload view =
    { knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }
