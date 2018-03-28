module Data.Editor.String
    exposing
        ( Editor
        , editor
        , saved
        , current
        , edit
        , cancel
        , update
        , isUpdated
        )


type Editor
    = Editor String String


editor : String -> Editor
editor new =
    Editor new new


saved : Editor -> String
saved e =
    case e of
        Editor _ saved ->
            saved


current : Editor -> String
current (Editor c _) =
    c


edit : String -> Editor -> Editor
edit newEdit (Editor _ saved) =
    Editor newEdit saved


cancel : Editor -> Editor
cancel (Editor _ saved) =
    editor saved


update : Editor -> Editor
update (Editor current saved) =
    editor current


isUpdated : Editor -> Bool
isUpdated (Editor current saved) =
    current == saved
