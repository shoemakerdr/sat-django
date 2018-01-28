module FloorPlanTypes exposing
  ( FloorPlan
  , Location
  , floorplanSample
  , locationListSample
  )



type alias FloorPlan =
  { id : Int
  , name : String
  , src : String
  , widthOffset : Float
  , heightOffset : Float
  }


type alias Location =
  { id : Int
  , name : String
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
  , src = "./floor-plan.jpg"
  , widthOffset = 1.0
  , heightOffset = 1.0
  }


newLocation : Int -> String -> Float -> Float -> Location
newLocation id name x y =
  { id = id
  , name = name
  , details = "Some details"
  , extension = "ext. 8000"
  , position_x = x
  , position_y = y
  , last_updated = 1517108599541
  }


locationListSample : List Location
locationListSample =
  [ newLocation 1 "bedroom" 0.175 0.2
  , newLocation 2 "bathroom" 0.4066666666666667 0.165
  , newLocation 3 "kitchen" 0.568333333333333 0.1325
  , newLocation 4 "living room" 0.68166 0.4275 
  ]
