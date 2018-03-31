module Request exposing (createChangeRequest)

import Data.FloorPlan exposing (FloorPlan, encodeFloorplan, decodeFloorplanAndLocations)
import Data.Location exposing (Location)
import Http
import HttpBuilder exposing (withHeaders, withJsonBody, withTimeout, withExpect)
import Time


type alias Token =
    String


createChangeRequest : Token -> FloorPlan -> List Location -> (Result Http.Error a -> msg) -> Cmd msg
createChangeRequest token floorplan locations requestHandler =
    HttpBuilder.post <|
        createApiUrl floorplan.id
            |> withHeader "X-CSRFToken" token
            |> withJsonBody (encodeFloorplan floorplan locations)
            |> withTimeout (10 * Time.second)
            |> withExpect (Http.expectJson decodeFloorplanAndLocations)
            |> HttpBuilder.send requestHandler


createApiUrl : Int -> String
createApiUrl id =
    "/api/floorplan/" ++ (toString id)
