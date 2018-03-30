module Data.Id
    exposing
        ( Id(..)
        , WithId
        , newId
        , oldId
        , isNew
        , nextId
        , equal
        )


type Id
    = New Int
    | Old Int


type alias WithId a =
    { a | id : Id }


newId : Int -> Id
newId i =
    New i


oldId : Int -> Id
oldId i =
    Old i


isNew : WithId a -> Bool
isNew wid =
    case wid.id of
        New _ ->
            True

        Old _ ->
            False


intFromId : Id -> Int
intFromId id =
    case id of
        New i ->
            i

        Old i ->
            i


nextId : List (WithId a) -> Id
nextId wids =
    wids
        |> List.filterMap
            (\w ->
                if isNew w then
                    Just <| intFromId w.id
                else
                    Nothing
            )
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1
        |> New


equal : Id -> Id -> Bool
equal l1 l2 =
    case ( l1, l2 ) of
        ( New i1, New i2 ) ->
            i1 == i2

        ( Old i1, Old i2 ) ->
            i1 == i2

        _ ->
            False
