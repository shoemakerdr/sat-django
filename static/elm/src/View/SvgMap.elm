module View.SvgMap exposing (view)

import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (class, id, style)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes as SvgAttr exposing (width, height, cx, cy, r, fill, fillOpacity)
import Data.Dimensions exposing (Dimensions)
import Data.Editor.List as LEditor
import Data.Filter as Filter exposing (Filter)
import Data.Location as Location exposing (Location)
import Data.FloorPlan as FloorPlan exposing (FloorPlan)
import Data.Mode exposing (..)
import Util exposing ((=>))


type alias Events msg =
    { mapEvents : List (Attribute msg)
    , locationEvents : Location -> List (Attribute msg)
    }


type alias Model m t =
    { m
        | floorplanDimensions : Maybe Dimensions
        , floorplan : FloorPlan
        , mode : Mode
        , locationEditor : LEditor.Editor Location
        , filters : List (Filter t Location)
    }


view : Events msg -> Model m t -> Html msg
view { mapEvents, locationEvents } model =
    case model.floorplanDimensions of
        Nothing ->
            div [] []

        Just dims ->
            div [ class "floorplan-map-wrapper" ]
                [ svg
                    ([ id "svg"
                     , width <| toString dims.width
                     , height <| toString dims.height
                     , style
                        [ "background" => ("url(" ++ "http://localhost:8000" ++ model.floorplan.image ++ ")")
                        , "backgroundSize" => "100% auto"
                        , "backgroundRepeat" => "no-repeat"
                        ]
                     ]
                        ++ mapEvents
                    )
                  <|
                    plotLocations locationEvents dims model
                ]


plotLocations : (Location -> List (Attribute msg)) -> Dimensions -> Model m t -> List (Html msg)
plotLocations events dimensions model =
    let
        filteredLocations =
            Filter.apply model.filters (LEditor.list model.locationEditor)

        locations =
            case model.mode of
                View ->
                    filteredLocations

                Edit _ ->
                    case LEditor.current model.locationEditor of
                        Nothing ->
                            filteredLocations

                        Just l ->
                            List.filter (not << (Location.equal l)) filteredLocations

        current =
            case model.mode of
                View ->
                    [ circle [ SvgAttr.visibility "collapse" ] [] ]

                Edit _ ->
                    LEditor.current model.locationEditor
                        |> Maybe.map
                            (\location ->
                                [ viewCircle events "#FF0000" "editing-location-point" model.mode dimensions location ]
                            )
                        |> Maybe.withDefault []
    in
        locations
            |> List.map
                (\location ->
                    viewCircle events "#72acdc" "location-point" model.mode dimensions location
                )
            |> (++) current


viewCircle : (Location -> List (Attribute msg)) -> String -> String -> Mode -> Dimensions -> Location -> Svg msg
viewCircle events color className mode dimensions location =
    circle
        ([ SvgAttr.class className
         , cx (toString <| location.position_x * dimensions.width)
         , cy (toString <| location.position_y * dimensions.height)
         , r "8"
         , fill color
         , fillOpacity "0.5"
         ]
            ++ events location
        )
        []
