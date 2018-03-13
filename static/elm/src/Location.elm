module Location
    exposing
        ( Location
        , equal
        , typeFromString
        , fromAbbr
        , fromReadable
        )

import Dict exposing (Dict)


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


type Id
    = New Int
    | Old Int


equal : Location -> Location -> Bool
equal l1 l2 =
    case ( .id l1, .id l2 ) of
        ( New i1, New i2 ) ->
            i1 == i2

        ( Old i1, Old i2 ) ->
            i1 == i2

        _ ->
            False


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
