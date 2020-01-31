module Settings exposing
    ( Orientation(..)
    , Settings
    , default
    , minDockHeight
    , minDockWidth
    , minNavigationWidth
    , minStoryHeight
    , minStoryWidth
    )


type Orientation
    = Horizontal
    | Vertical


type alias Settings =
    { fullscreen : Bool
    , navigationVisible : Bool
    , navigationWidth : Int
    , dockVisible : Bool
    , dockWidth : Int
    , dockHeight : Int
    , dockOrientation : Orientation
    , addPaddings : Bool
    , darkBackground : Bool
    , showGrid : Bool
    }


default : Settings
default =
    { fullscreen = False
    , navigationVisible = True
    , navigationWidth = 200
    , dockVisible = True
    , dockWidth = 400
    , dockHeight = 300
    , dockOrientation = Horizontal
    , addPaddings = False
    , darkBackground = False
    , showGrid = False
    }


minNavigationWidth : Int
minNavigationWidth =
    100


minDockWidth : Int
minDockWidth =
    300


minDockHeight : Int
minDockHeight =
    200


minStoryWidth : Int
minStoryWidth =
    300


minStoryHeight : Int
minStoryHeight =
    200
