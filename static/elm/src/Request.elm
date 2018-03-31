module Request exposing (createChangeRequest)

import Data.FloorPlan
    exposing
        ( FloorPlan
        , FloorPlanDataPair
        , encodeFloorplan
        , decodeFloorplanAndLocations
        )
import Data.Location exposing (Location)
import Http
import HttpBuilder
    exposing
        ( RequestBuilder
        , withHeaders
        , withJsonBody
        , withTimeout
        , withExpect
        )
import Time


type alias Token =
    String


type alias DataPairHandler =
    Result Http.Error FloorPlanDataPair -> msg


post : Token -> FloorPlan -> List Location -> Http.Request FloorPlanDataPair
post token floorplan locations =
    HttpBuilder.post <|
        createApiUrl floorplan.id
            |> withHeader "X-CSRFToken" token
            |> withJsonBody (encodeFloorplan floorplan locations)
            |> withTimeout (10 * Time.second)
            |> withExpect (Http.expectJson decodeFloorplanAndLocations)
            |> HttpBuilder.toRequest


send : DataPairHandler -> Http.Request FloorPlanDataPair -> Cmd msg
send handler request =
    Http.send handler request


createApiUrl : Int -> String
createApiUrl id =
    "/api/floorplan/" ++ (toString id)
