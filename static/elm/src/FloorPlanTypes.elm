module FloorPlanTypes
    exposing
        ( FloorPlan
        , Location
        , newFloorPlan
        , locMapAbbrToReadable
        , locMapReadableToAbbr
        , getLocationFromAbbr
        , getLocationFromReadable
        )

import Dict exposing (Dict, fromList, get)


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
    , owner_name = flag.owner_name
    , last_updated = flag.last_updated
    }


locMapAbbrToReadable : Dict String String
locMapAbbrToReadable =
    fromList
        [ ( "DESK", "Desk" )
        , ( "OFFICE", "Office" )
        , ( "CONFR", "Conference Room" )
        , ( "COMMON", "Common Area" )
        , ( "RESTROOM", "Restroom" )
        , ( "PUBLIC", "Public Area" )
        , ( "PRIVATE", "Private Area" )
        , ( "MISC", "Miscellaneous" )
        ]


locMapReadableToAbbr : Dict String String
locMapReadableToAbbr =
    fromList
        [ ( "Desk", "DESK" )
        , ( "Office", "OFFICE" )
        , ( "Conference Room", "CONFR" )
        , ( "Common Area", "COMMON" )
        , ( "Restroom", "RESTROOM" )
        , ( "Public Area", "PUBLIC" )
        , ( "Private Area", "PRIVATE" )
        , ( "Miscellaneous", "MISC" )
        ]


getLocation : String -> Dict String String -> String
getLocation loc locDict =
    let
        l =
            get loc locDict
    in
        Maybe.withDefault "" l


getLocationFromAbbr : String -> String
getLocationFromAbbr loc =
    getLocation loc locMapAbbrToReadable


getLocationFromReadable : String -> String
getLocationFromReadable loc =
    getLocation loc locMapReadableToAbbr
