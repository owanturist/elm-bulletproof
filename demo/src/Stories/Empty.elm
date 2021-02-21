module Stories.Empty exposing (story)

import Bulletproof
import Empty


story : Bulletproof.Story
story =
    Bulletproof.story "Empty" Empty.view
