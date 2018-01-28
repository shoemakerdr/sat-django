import Html exposing (Html, div, text, img, p, a, h1)
import Html.Attributes exposing (src, alt, class, style)
import Html.Events exposing (onClick)
import Svg exposing (svg, circle)
import Svg.Attributes as SvgAttr exposing (width, height, cx, cy, r, fill, fillOpacity)

import FloorPlanTypes exposing (..)



main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  (Model floorplanSample locationListSample) ! []



-- MODEL


type alias Model =
  { floorplan : FloorPlan
  , locations : List Location
  }



-- UPDATE


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []



-- VIEW


view : Model -> Html Msg
view model =
  div [ onClick NoOp ]
    [ text <| "The name of this floor plan is " ++ model.floorplan.name
    , svgMap model.floorplan model.locations
    ]


svgMap : FloorPlan -> List Location -> Html Msg
svgMap floorplan locations =
  svg
    [ width "600"
    , height "400"
    , style [ ("background", "url(" ++ floorplan.src ++ ")"), ("backgroundSize", "100% auto"), ("backgroundRepeat", "no-repeat") ] 
    ] <|
      plotLocations locations


plotLocations : List Location -> List (Html Msg)
plotLocations locations =
  locations
    |> List.map plotLocation


plotLocation : Location -> Html Msg
plotLocation location =
  circle
    [ cx (toString <| location.position_x * 600)
    , cy (toString <| location.position_y * 400)
    , r "8"
    , fill "blue"
    , fillOpacity "0.5"
    ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP



-- DECODER

