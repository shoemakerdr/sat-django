module Data.FloorPlan
    exposing
        ( FloorPlan
        , Flag
        , FloorPlanDataPair
        , floorplan
        , encodeFloorplan
        , dataPairDecoder
        )

import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as Pipeline
import Data.Location exposing (Location, encodeLocations, decodeLocations, locationsFromFieldDecoder)
import Util exposing ((=>))


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
    { a | locations : JD.Value }


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



-- DECODERS


type alias FloorPlanDataPair =
    ( FloorPlan, List Location )


dataPairDecoder : JD.Decoder FloorPlanDataPair
dataPairDecoder =
    JD.map2 (,) floorplanDecoder locationsFromFieldDecoder


floorplanDecoder : JD.Decoder FloorPlan
floorplanDecoder =
    Pipeline.decode FloorPlan
        |> Pipeline.required "id" JD.int
        |> Pipeline.required "aspect_ratio" JD.float
        |> Pipeline.required "image" JD.string
        |> Pipeline.required "is_public" JD.bool
        |> Pipeline.required "is_trashed" JD.bool
        |> Pipeline.required "name" JD.string
        |> Pipeline.required "owner" JD.int
        |> Pipeline.required "owner_name" JD.string
        |> Pipeline.required "last_updated" JD.string



-- ENCODERS


encodeFloorplan : FloorPlan -> List Location -> JE.Value
encodeFloorplan floorplan locations =
    JE.object
        [ "id" => JE.int floorplan.id
        , "is_public" => JE.bool floorplan.is_public
        , "is_trashed" => JE.bool floorplan.is_trashed
        , "name" => JE.string floorplan.name
        , "owner" => JE.int floorplan.owner
        , "owner_name" => JE.string floorplan.owner_name
        , "locations" => encodeLocations locations
        ]
