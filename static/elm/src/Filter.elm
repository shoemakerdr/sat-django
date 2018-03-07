module Filter
    exposing
        ( Filter
        , new
        , merge
        , remove
        , apply
        )


type Filter a b
    = Filter a (b -> Bool)


new : a -> (b -> Bool) -> Filter a b
new t predicate =
    Filter t predicate


merge : Filter a b -> List (Filter a b) -> List (Filter a b)
merge filter filterList =
    filterList
        |> remove filter
        |> (::) filter


remove : Filter a b -> List (Filter a b) -> List (Filter a b)
remove filter filterList =
    filterList
        |> List.filter
            (\f ->
                case ( filter, f ) of
                    ( Filter a _, Filter b _ ) ->
                        a /= b
            )


apply : List (Filter a b) -> List b -> List b
apply filters list =
    case List.head filters of
        Nothing ->
            list

        Just (Filter _ predicate) ->
            apply (List.drop 1 filters) <| List.filter predicate list
