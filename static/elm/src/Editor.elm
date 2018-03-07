module Editor
    exposing
        ( Editor
        , editor
        , save
        , update
        , (<::>)
        )

import List.Extra as Extra


type Editor a
    = Saved (List a)
    | Updating (Maybe a) (List a)
    | Adding (Maybe a) (List a)
    | Deleting (List a)


editor : List a -> Editor a
editor list =
    Saved list


save : Editor a -> List a
save e =
    case e of
        Saved list ->
            list

        Updating _ list ->
            list

        Adding _ list ->
            list

        Deleting list ->
            list


update : (a -> Bool) -> Maybe a -> List a -> Editor a
update predicate e list =
    case e of
        Nothing ->
            Updating Nothing list

        Just item ->
            Updating Nothing (Extra.updateIf predicate (\_ -> item) list)


(<::>) : Maybe a -> List a -> List a
(<::>) item list =
    case item of
        Nothing ->
            list

        Just i ->
            i :: list
