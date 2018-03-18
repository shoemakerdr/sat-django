module ToolTip
    exposing
        ( ToolTip(..)
        , show
        , hide
        , move
        , config
        , view
        )

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Util exposing ((=>))
import Mouse exposing (Position)


-- MODEL


type ToolTip
    = Hidden (Maybe Position)
    | Shown (Maybe Position)


show : ToolTip -> ToolTip
show toolTip =
    case toolTip of
        Hidden pos ->
            Shown pos

        Shown _ ->
            toolTip


hide : ToolTip -> ToolTip
hide toolTip =
    case toolTip of
        Hidden _ ->
            toolTip

        Shown pos ->
            Hidden pos


move : Position -> ToolTip -> ToolTip
move pos toolTip =
    case toolTip of
        Hidden _ ->
            Hidden (Just pos)

        Shown _ ->
            Shown (Just pos)



-- VIEW


type alias Config =
    { xOffset : Int
    , yOffset : Int
    , wrapperClasses : String
    , additionalStyles : List ( String, String )
    }


config : Int -> Int -> String -> List ( String, String ) -> Config
config x y classes styles =
    { xOffset = x
    , yOffset = y
    , wrapperClasses = classes
    , additionalStyles = styles
    }


view : Config -> Html msg -> ToolTip -> Html msg
view config html toolTip =
    case toolTip of
        Shown (Just pos) ->
            let
                styles =
                    ([ "top" => px (pos.y + config.yOffset)
                     , "left" => px (pos.x + config.xOffset)
                     ]
                        ++ config.additionalStyles
                    )
            in
                div
                    [ class config.wrapperClasses
                    , style styles
                    ]
                    [ html ]

        _ ->
            div [] []


px : Int -> String
px i =
    toString i ++ "px"
