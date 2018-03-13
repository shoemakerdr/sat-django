module Data.FloorPlan
    exposing
        ( FloorPlan
        , Flag
        , floorplan
        )

import Json.Decode as Json


type alias FloorPlan =
    { id : Int
    , aspect_ratio : Float
    , image : String
    , is_public : Bool
    , is_trashed : Bool
    , name : String
    , owner : Int
    , owner_name : String
    , last_updated : String
    }


type alias Flag a =
    { a | locations : Json.Value }


floorplan : Flag FloorPlan -> FloorPlan
floorplan flag =
    { id = flag.id
    , aspect_ratio = flag.aspect_ratio
    , image = flag.image
    , is_public = flag.is_public
    , is_trashed = flag.is_trashed
    , name = flag.name
    , owner = flag.owner
    , owner_name = flag.owner_name
    , last_updated = flag.last_updated
    }
