module Data.Dimensions exposing (Dimensions, get)

import Window exposing (Size)


type alias Dimensions =
    { width : Float
    , height : Float
    }


type alias AspectRatio =
    -- AspectRatio is a ratio of width to height
    -- Ex.: If width is 1000 and height is 800, the AspectRatio
    -- would be 0.8
    Float


type alias Percentage =
    Float


get : Percentage -> Size -> AspectRatio -> Dimensions
get percentage size aspect_ratio =
    let
        width =
            percentage * (toFloat size.width)
    in
        { width = width
        , height = width * aspect_ratio
        }
