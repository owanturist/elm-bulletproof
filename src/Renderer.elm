module Renderer exposing (Renderer(..), unwrap)

import Html.Styled exposing (Html)


type Renderer
    = Renderer (Html ())


unwrap : Renderer -> Html ()
unwrap (Renderer layout) =
    layout
