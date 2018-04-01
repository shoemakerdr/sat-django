module View.Options exposing (view)

import Data.Location as Location
import Html exposing (Html, option, text)
import Html.Attributes exposing (selected)


view : String -> Bool -> List (Html msg)
view typeSelected useInitialOption =
    let
        opts =
            Location.readableTypes

        initialOption =
            if useInitialOption then
                option
                    [ selected <| typeSelected == Location.noSelection ]
                    [ text Location.noSelection ]
                    :: []
            else
                []
    in
        (++) initialOption
            (opts
                |> List.map
                    (\opt ->
                        option [ selected <| typeSelected == opt ] [ text opt ]
                    )
            )
