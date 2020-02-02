module Stories.Error exposing (story)

import Bulletproof
import Bulletproof.Knob
import Error


story : Bulletproof.Story
story =
    Bulletproof.folderOf "Error"
        [ Bulletproof.todo "Empty Label Title"
        , Bulletproof.todo "Empty Story Title"
        , Bulletproof.todo "Empty Todo Title"
        , Bulletproof.todo "Empty Folder Title"
        , Bulletproof.todo "Empty Knob Title"
        , Bulletproof.todo "Empty Radio"
        , Bulletproof.todo "Empty Select"
        , Bulletproof.todo "Duplicate Labels"
        , Bulletproof.todo "Duplicate Stories"
        , Bulletproof.todo "Duplicate Folders"
        , Bulletproof.todo "Duplicate Knob"
        , Bulletproof.todo "Duplicate RadioOptions"
        , Bulletproof.todo "Duplicate SelectOptions"
        , Bulletproof.todo "Invalid Int Step"
        , Bulletproof.todo "Invalid Int Left Boundary"
        , Bulletproof.todo "Invalid Int Right Boundary"
        , Bulletproof.todo "Invalid Int Boundaries"
        , Bulletproof.todo "Invalid Float Step"
        , Bulletproof.todo "Invalid Float Left Boundary"
        , Bulletproof.todo "Invalid Float Right Boundary"
        , Bulletproof.todo "Invalid Float Boundaries"
        , Bulletproof.todo "Invalid Color"
        , Bulletproof.todo "Invalid Date"
        , Bulletproof.todo "Invalid Time"
        ]
