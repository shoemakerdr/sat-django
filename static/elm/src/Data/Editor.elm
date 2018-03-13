module Data.Editor
    exposing
        ( Editor
        , editor
        , retrieve
        , current
        , edit
        , maybeEdit
        , cancel
        , update
        , create
        , delete
        , newWithDefault
        , isActive
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


current : Editor a -> Maybe a
current (Editor c _) =
    c


edit : a -> Editor a -> Editor a
edit new (Editor _ list) =
    Editor (Just new) list


maybeEdit : Maybe a -> Editor a -> Editor a
maybeEdit new (Editor _ list) =
    Editor new list


cancel : Editor a -> Editor a
cancel (Editor _ list) =
    editor list


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


newWithDefault : (a -> Editor a -> Editor a) -> Editor a -> Editor a
newWithDefault func editor =
    case current editor of
        Nothing ->
            editor

        Just c ->
            func c editor


isActive : Editor a -> Bool
isActive (Editor current _) =
    case current of
        Nothing ->
            (Debug.log "isActive" False)

        Just _ ->
            (Debug.log "isActive" True)
