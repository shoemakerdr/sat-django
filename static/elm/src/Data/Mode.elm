module Data.Mode exposing (Mode(..), Status(..))


type Mode
    = View
    | Edit Status


type Status
    = Waiting
    | Adding
    | FloorPlanName
    | Editing
    | PreparingForMove
    | Moving
    | DeleteConfirmation
    | InvalidLocation
