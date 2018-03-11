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


type ToolTip model
    = Hidden (Maybe Position) (Maybe model)
    | Shown (Maybe Position) (Maybe model)


show : ToolTip model -> ToolTip model
show toolTip =
    case toolTip of
        Hidden pos model ->
            Shown pos model

        Shown _ _ ->
            toolTip


hide : ToolTip model -> ToolTip model
hide toolTip =
    case toolTip of
        Hidden _ _ ->
            toolTip

        Shown pos model ->
            Hidden pos model


move : Position -> ToolTip model -> ToolTip model
move pos toolTip =
    case toolTip of
        Hidden _ model ->
            Hidden (Just pos) model

        Shown _ model ->
            Shown (Just pos) model



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


view : Config -> (model -> Html msg) -> ToolTip model -> Html msg
view config toHtml toolTip =
    case toolTip of
        Shown (Just pos) (Just model) ->
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
                    [ toHtml model ]

        _ ->
            div [] []


px : Int -> String
px i =
    toString i ++ "px"
