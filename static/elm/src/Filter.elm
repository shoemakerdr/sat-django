module Filter exposing
  ( Filter(..)
  , merge
  , applyFilters
  )


type Filter a b = Filter a (b -> Bool)


merge : Filter a b -> List (Filter a b) -> List (Filter a b)
merge filter filterList =
  filterList
    |> List.filter
        (\f ->
          case (filter, f) of
            (Filter a _, Filter b _) ->
              a /= b
        )
    |> (::) filter


apply : List (Filter a b) -> List b -> List b
apply filters list =
  case List.head filters of
    Nothing ->
      list

    Just (Filter _ predicate) ->
      apply (List.drop 1 filters) <| List.filter predicate list


