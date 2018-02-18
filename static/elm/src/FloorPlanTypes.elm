module FloorPlanTypes
    exposing
        ( FloorPlan
        , Location
        , newFloorPlan
        )


type alias FloorPlan =
    { id : Int
    , aspect_ratio : Float
    , image : String
    , is_public : Bool
    , is_trashed : Bool
    , name : String
    , owner : Int
    , last_updated : String
    }


type alias Location =
    { id : Int
    , floorplan : Int
    , name : String
    , loc_type : String
    , details : String
    , extension : Maybe Int
    , is_trashed : Bool
    , position_x : Float
    , position_y : Float
    , last_updated : String
    }


type alias Flag a =
    { a | locations : List Location }


newFloorPlan : Flag FloorPlan -> FloorPlan
newFloorPlan flag =
    { id = flag.id
    , aspect_ratio = flag.aspect_ratio
    , image = flag.image
    , is_public = flag.is_public
    , is_trashed = flag.is_trashed
    , name = flag.name
    , owner = flag.owner
    , last_updated = flag.last_updated
    }
