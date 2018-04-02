module Request exposing (saveDataPair)

import Data.FloorPlan
    exposing
        ( FloorPlan
        , FloorPlanDataPair
        , encodeFloorplan
        , dataPairDecoder
        )
import Data.Location exposing (Location)
import Http
import HttpBuilder
    exposing
        ( RequestBuilder
        , withHeader
        , withJsonBody
        , withTimeout
        , withExpect
        )
import Time


type alias Token =
    String


type alias DataPairHandler msg =
    Result Http.Error FloorPlanDataPair -> msg


saveDataPair : Token -> FloorPlan -> List Location -> DataPairHandler msg -> Cmd msg
saveDataPair token floorplan locations handler =
    HttpBuilder.post (createApiUrl floorplan.id)
        |> withHeader "X-CSRFToken" token
        |> withJsonBody (encodeFloorplan floorplan locations)
        |> withTimeout (10 * Time.second)
        |> withExpect (Http.expectJson dataPairDecoder)
        |> HttpBuilder.send handler


createApiUrl : Int -> String
createApiUrl id =
    "http://localhost:8000/api/floorplans/" ++ (toString id) ++ "/"
