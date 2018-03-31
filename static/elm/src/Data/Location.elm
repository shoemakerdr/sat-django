module Data.Location
    exposing
        ( Location
        , new
        , equal
        , isValid
        , typeFromString
        , readableTypes
        , abbrTypes
        , fromAbbr
        , fromReadable
        , extensionToString
        , noSelection
        , decodeLocations
        , encodeLocations
        )

import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as Pipeline
import Dict exposing (Dict)
import Util exposing ((=>))
import Data.Id as Id exposing (Id(..))


type alias Location =
    { id : Id
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


new : Id -> Int -> Location
new id floorplanId =
    { id = id
    , floorplan = floorplanId
    , name = ""
    , loc_type = "DESK"
    , details = ""
    , extension = Nothing
    , is_trashed = False
    , position_x = 0
    , position_y = 0
    , last_updated = ""
    }


equal : Location -> Location -> Bool
equal l1 l2 =
    let
        id1 =
            .id l1

        id2 =
            .id l2
    in
        Id.equal id1 id2


isValid : Location -> Bool
isValid location =
    location.name /= ""


readableTypes : List String
readableTypes =
    [ "Desk"
    , "Office"
    , "Conference Room"
    , "Common Area"
    , "Restroom"
    , "Public Area"
    , "Private Area"
    , "Miscellaneous"
    ]


abbrTypes : List String
abbrTypes =
    [ "DESK"
    , "OFFICE"
    , "CONFR"
    , "COMMON"
    , "RESTROOM"
    , "PUBLIC"
    , "PRIVATE"
    , "MISC"
    ]


abbrToReadable : Dict String String
abbrToReadable =
    Dict.fromList <|
        List.map2 (,) abbrTypes readableTypes


readableToAbbr : Dict String String
readableToAbbr =
    Dict.fromList <|
        List.map2 (,) readableTypes abbrTypes


extensionToString : Maybe Int -> String
extensionToString ext =
    case ext of
        Nothing ->
            ""

        Just x ->
            toString x


typeFromString : String -> Dict String String -> String
typeFromString loc locDict =
    let
        l =
            Dict.get loc locDict
    in
        Maybe.withDefault "" l


fromAbbr : String -> String
fromAbbr loc =
    typeFromString loc abbrToReadable


fromReadable : String -> String
fromReadable loc =
    typeFromString loc readableToAbbr


noSelection : String
noSelection =
    "-- Select type --"



-- DECODERS


decodeLocations : JD.Value -> Result String (List Location)
decodeLocations value =
    JD.decodeValue locationsDecoder value


locationsDecoder : JD.Decoder (List Location)
locationsDecoder =
    JD.list locationDecoder


locationDecoder : JD.Decoder Location
locationDecoder =
    Pipeline.decode Location
        |> Pipeline.required "id" idDecoder
        |> Pipeline.required "floorplan" JD.int
        |> Pipeline.required "name" JD.string
        |> Pipeline.required "loc_type" JD.string
        |> Pipeline.required "details" JD.string
        |> Pipeline.required "extension" (JD.nullable JD.int)
        |> Pipeline.required "is_trashed" JD.bool
        |> Pipeline.required "position_x" JD.float
        |> Pipeline.required "position_y" JD.float
        |> Pipeline.required "last_updated" JD.string


idDecoder : JD.Decoder Id
idDecoder =
    JD.int
        |> JD.andThen (\i -> JD.succeed (Id.oldId i))



-- ENCODERS


encodeLocations : List Location -> JE.Value
encodeLocations locations =
    JE.list <|
        List.map encodeLocation locations


encodeLocation : Location -> JE.Value
encodeLocation location =
    let
        idField =
            case location.id of
                New _ ->
                    []

                Old i ->
                    [ "id" => JE.int i ]
    in
        JE.object <|
            idField
                ++ [ "name" => JE.string location.name
                   , "floorplan" => JE.int location.floorplan
                   , "loc_type" => JE.string location.loc_type
                   , "details" => JE.string location.details
                   , "extension" => encodeMaybeWith JE.int location.extension
                   , "position_x" => JE.float location.position_x
                   , "position_y" => JE.float location.position_y
                   , "is_trashed" => JE.bool location.is_trashed
                   ]


encodeMaybeWith : (a -> JE.Value) -> Maybe a -> JE.Value
encodeMaybeWith encoder val =
    case val of
        Nothing ->
            JE.null

        Just v ->
            encoder v
