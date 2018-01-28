import Html exposing
  ( Html
  , Attribute
  , div
  , text
  , p
  , h1
  , button
  , input
  , select
  , option
  )
import Html.Attributes exposing
  ( src
  , alt
  , class
  , style
  , disabled
  , selected
  , placeholder
  , value
  )
import Html.Events exposing (onClick, on, targetValue)
import Svg exposing (svg, circle)
import Svg.Attributes as SvgAttr exposing
  ( width
  , height
  , cx
  , cy
  , r
  , fill
  , fillOpacity
  )
import Json.Decode as Json

import FloorPlanTypes exposing (..)



main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


defaultSelect : String
defaultSelect = "-- Select type --"


init : (Model, Cmd Msg)
init =
  { floorplan = floorplanSample
  , locations  = locationListSample
  , filterNameInput = ""
  , filterTypeSelect = defaultSelect
  , toolTip = NotShown
  , filteredLocations = locationListSample
  , searchFilter = NoFilter
  } ! []



-- MODEL


type alias Model =
  { floorplan : FloorPlan
  , locations : List Location
  , filterNameInput : String
  , filterTypeSelect : String
  , toolTip : ToolTip
  , filteredLocations : List Location
  , searchFilter : Filter
  }


type ToolTip
  = NotShown
  | SmallToolTip String
  | DetailedToolTip Location


type Filter
  = NoFilter
  | ByName String
  | ByType String
  | ByNameAndType String String



-- UPDATE


type Msg
  = FilterLocations Filter
  | NameInputChange String
  | TypeSelectChange String
  | ResetFilterForm


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FilterLocations _ ->
      model ! []

    NameInputChange name ->
      { model | filterNameInput = name } ! []

    TypeSelectChange locationType ->
      { model | filterTypeSelect = locationType } ! []

    ResetFilterForm ->
      { model
      | filterNameInput = ""
      , filterTypeSelect = defaultSelect
      , filteredLocations = model.locations
      , searchFilter = NoFilter
      } ! []


-- VIEW


(=>) = (,)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" (Json.map tagger targetValue) 


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text model.floorplan.name ]
    , div
        [ style
            [ "display" => "flex"
            , "justify-content" => "space-around"
            , "align-items" => "flex-start"
            ]
        ]
        [ svgMap model.floorplan model.filteredLocations
        , viewFilterLocations model
        ]
    ]


svgMap : FloorPlan -> List Location -> Html Msg
svgMap floorplan locations =
  svg
    [ width "600"
    , height "400"
    , style
        [ "background" => ("url(" ++ floorplan.src ++ ")")
        , "backgroundSize" => "100% auto"
        , "backgroundRepeat" => "no-repeat"
        ]
    ]
    <| plotLocations locations


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
    , fill "gray"
    , fillOpacity "0.5"
    ] []


viewFilterLocations : Model -> Html Msg
viewFilterLocations { filteredLocations, filterNameInput, filterTypeSelect } =
  div []
    [ h1 [] [ text "Locations" ]
    , filterForm filterNameInput filterTypeSelect
    , div [] <| locationInfoList filteredLocations
    ]


filterForm : String -> String -> Html Msg
filterForm nameInput typeSelected =
  div []
    [ input
        [ placeholder "Filter by name"
        , value nameInput
        , onChange NameInputChange
        ] []
    , select [ onChange TypeSelectChange ] <| optionList typeSelected
    , button [ onClick ResetFilterForm ] [ text "Reset filter" ]
    ]


optionList : String -> List (Html Msg)
optionList typeSelected =
  let
    options =
      [ "Desk"
      , "Office"
      , "Conference Room"
      , "Common Area"
      , "Restroom"
      , "Public Area"
      , "Private Area"
      , "Miscellaneous"
      ]
    initialOption =
      option
        [ selected <| isTypeSelected typeSelected defaultSelect ]
        [ text defaultSelect ] :: []
  in
    (++) initialOption
      (options
        |> List.map
          (\opt ->
              option [ selected <| isTypeSelected typeSelected opt ] [ text opt ]
          )
      )


isTypeSelected : String -> String -> Bool
isTypeSelected typeSelected t =
  typeSelected == t

locationInfoList : List Location -> List (Html Msg)
locationInfoList locations =
  locations
    |> List.map
      (\location ->
        p [] [ text <| location.name ++ " - " ++ location.locationType ])



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP



-- DECODER

