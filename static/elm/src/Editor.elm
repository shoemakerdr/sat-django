module Editor
    exposing
        ( Editor
        , editor
        , retrieve
        , edit
        , update
        , create
        , delete
        )

import List.Extra as Extra


type Editor a
    = Editor (Maybe a) (List a)


editor : List a -> Editor a
editor list =
    Editor Nothing list


retrieve : Editor a -> List a
retrieve e =
    case e of
        Editor _ list ->
            list


edit : a -> Editor a -> Editor a
edit new (Editor _ list) =
    Editor (Just new) list


update : (a -> Bool) -> Editor a -> Editor a
update predicate (Editor current list) =
    case current of
        Nothing ->
            editor list

        Just c ->
            editor (Extra.updateIf predicate (\_ -> c) list)


create : Editor a -> Editor a
create (Editor current list) =
    case current of
        Nothing ->
            editor list

        Just c ->
            editor (c :: list)


delete : (a -> Bool) -> Editor a -> Editor a
delete predicate (Editor current list) =
    case current of
        Nothing ->
            editor list

        Just c ->
            editor (List.filter (not << predicate) list)
