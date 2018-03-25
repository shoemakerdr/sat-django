module Id exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Id as Id exposing (Id)


suite : Test
suite =
    describe "Id module"
        [ describe "Id.nextId"
            [ test "should return an Id of New 1 with empty list" <|
                \_ ->
                    let
                        newId =
                            Id.nextId []
                    in
                        Expect.equal newId (Id.newId 1)
            , test "should return an Id of New 2 with list of 1 WithId with Id of New 1" <|
                \_ ->
                    let
                        newId =
                            Id.nextId [ { id = Id.newId 1 } ]
                    in
                        Expect.equal newId (Id.newId 2)
            , test "should return an Id of New 1 with list of 1 WithId with Id of Old 1" <|
                \_ ->
                    let
                        newId =
                            Id.nextId [ { id = Id.oldId 1 } ]
                    in
                        Expect.equal newId (Id.newId 1)
            ]
        ]
