module FloorPlanTypes
    exposing
        ( FloorPlan
        , Location
        , floorplanSample
        , locationListSample
        )


type alias FloorPlan =
    { id : Int
    , name : String
    , src : String
    , widthRatio : Float
    , heightRatio : Float
    }


type alias Location =
    { id : Int
    , name : String
    , locationType : String
    , details : String
    , extension : String
    , position_x : Float
    , position_y : Float
    , last_updated : Int
    }


floorplanSample : FloorPlan
floorplanSample =
    { id = 1
    , name = "My House"
    , src = "src/floor-plan.jpg"
    , widthRatio = 1.0
    , heightRatio = 0.6667
    }


newLocation : Int -> String -> String -> Float -> Float -> Location
newLocation id name locationType x y =
    { id = id
    , name = name
    , locationType = locationType
    , details = "Some details"
    , extension = "8000"
    , position_x = x
    , position_y = y
    , last_updated = 1517108599541
    }


locationListSample : List Location
locationListSample =
    [ newLocation 1 "bedroom" "Private Area" 0.175 0.2
    , newLocation 2 "bathroom" "Restroom" 0.4066666666666667 0.165
    , newLocation 3 "kitchen" "Common Area" 0.568333333333333 0.1325
    , newLocation 4 "living room" "Common Area" 0.68166 0.4275
    ]
