module Data.Location
    exposing
        ( Location
        , new
        , equal
        , isValid
        , typeFromString
        , fromAbbr
        , fromReadable
        , extensionToString
        , decodeLocations
        )

import Json.Decode as Json
import Json.Decode.Pipeline as Pipeline
import Dict exposing (Dict)
import Data.Id as Id exposing (Id)


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


abbrToReadable : Dict String String
abbrToReadable =
    Dict.fromList
        [ ( "DESK", "Desk" )
        , ( "OFFICE", "Office" )
        , ( "CONFR", "Conference Room" )
        , ( "COMMON", "Common Area" )
        , ( "RESTROOM", "Restroom" )
        , ( "PUBLIC", "Public Area" )
        , ( "PRIVATE", "Private Area" )
        , ( "MISC", "Miscellaneous" )
        ]


readableToAbbr : Dict String String
readableToAbbr =
    Dict.fromList
        [ ( "Desk", "DESK" )
        , ( "Office", "OFFICE" )
        , ( "Conference Room", "CONFR" )
        , ( "Common Area", "COMMON" )
        , ( "Restroom", "RESTROOM" )
        , ( "Public Area", "PUBLIC" )
        , ( "Private Area", "PRIVATE" )
        , ( "Miscellaneous", "MISC" )
        ]


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


decodeLocations : Json.Value -> List Location
decodeLocations value =
    case Json.decodeValue locationsDecoder value of
        Ok locations ->
            locations

        -- probably shouldn't Debug.crash
        Err err ->
            Debug.crash err


locationsDecoder : Json.Decoder (List Location)
locationsDecoder =
    Json.list locationDecoder


locationDecoder : Json.Decoder Location
locationDecoder =
    Pipeline.decode Location
        |> Pipeline.required "id" idDecoder
        |> Pipeline.required "floorplan" Json.int
        |> Pipeline.required "name" Json.string
        |> Pipeline.required "loc_type" Json.string
        |> Pipeline.required "details" Json.string
        |> Pipeline.required "extension" (Json.nullable Json.int)
        |> Pipeline.required "is_trashed" Json.bool
        |> Pipeline.required "position_x" Json.float
        |> Pipeline.required "position_y" Json.float
        |> Pipeline.required "last_updated" Json.string


idDecoder : Json.Decoder Id
idDecoder =
    Json.int
        |> Json.andThen (\i -> Json.succeed (Id.oldId i))
