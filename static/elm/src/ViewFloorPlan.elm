module Main exposing (..)

import Html
    exposing
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
import Html.Attributes
    exposing
        ( src
        , alt
        , class
        , style
        , disabled
        , selected
        , placeholder
        , value
        )
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave, on, targetValue)
import Svg exposing (svg, circle)
import Svg.Attributes as SvgAttr
    exposing
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
import Filter exposing (Filter(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


defaultSelect : String
defaultSelect =
    "-- Select type --"


init : ( Model, Cmd Msg )
init =
    { floorplan = floorplanSample
    , locations = locationListSample
    , nameInput = ""
    , typeSelect = defaultSelect
    , toolTip = Hidden
    , filteredLocations = locationListSample
    , filters = []
    }
        ! []



-- MODEL


type alias Model =
    { floorplan : FloorPlan
    , locations : List Location
    , nameInput : String
    , typeSelect : String
    , toolTip : ToolTip
    , filteredLocations : List Location
    , filters : List (Filter FilterType Location)
    }


type ToolTip
    = Hidden
    | Showing Location


type alias Position =
    { x : Float, y : Float }


type FilterType
    = Name
    | Type


type FilterMsg
    = Merge
    | Remove



-- UPDATE


type Msg
    = NameInputChange String
    | TypeSelectChange String
    | ResetFilterForm
    | ShowToolTip Location
    | HideToolTip


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInputChange name ->
            let
                filterMsg =
                    case name of
                        "" ->
                            Remove

                        _ ->
                            Merge
            in
                updateFilter filterMsg (Filter Name (filterByName name)) { model | nameInput = name }

        TypeSelectChange locationType ->
            let
                filterMsg =
                    if locationType == defaultSelect then
                        Remove
                    else
                        Merge
            in
                updateFilter filterMsg (Filter Type (filterByType locationType)) { model | typeSelect = locationType }

        ResetFilterForm ->
            { model
                | nameInput = ""
                , typeSelect = defaultSelect
                , filteredLocations = model.locations
                , filters = []
            }
                ! []

        ShowToolTip location ->
            { model | toolTip = Showing location } ! []

        HideToolTip ->
            { model | toolTip = Hidden } ! []


filterByName : String -> Location -> Bool
filterByName name location =
    String.contains (String.toLower name) (String.toLower location.name)


filterByType : String -> Location -> Bool
filterByType locationType location =
    locationType == location.locationType


updateFilter : FilterMsg -> Filter FilterType Location -> Model -> ( Model, Cmd Msg )
updateFilter filterMsg filter model =
    let
        filters =
            case filterMsg of
                Remove ->
                    Filter.remove filter model.filters

                Merge ->
                    Filter.merge filter model.filters
    in
        { model
            | filteredLocations =
                Filter.apply filters model.locations
            , filters = filters
        }
            ! []



-- VIEW


(=>) =
    (,)


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
        , viewToolTip model.toolTip
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
    <|
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
        , fill "gray"
        , fillOpacity "0.5"
        , onMouseEnter (ShowToolTip location)
        , onMouseLeave HideToolTip
        ]
        []


viewToolTip : ToolTip -> Html Msg
viewToolTip toolTip =
    case toolTip of
        Hidden ->
            div [] []

        Showing location ->
            div
                []
                [ text <|
                    "The "
                        ++ location.name
                        ++ " is a "
                        ++ location.locationType
                        ++ ". Details:  "
                        ++ location.details
                ]


viewFilterLocations : Model -> Html Msg
viewFilterLocations { filteredLocations, nameInput, typeSelect } =
    div []
        [ h1 [] [ text "Locations" ]
        , filterForm nameInput typeSelect
        , div [] <| locationInfoList filteredLocations
        ]


filterForm : String -> String -> Html Msg
filterForm nameInput typeSelected =
    div []
        [ input
            [ placeholder "Filter by name"
            , value nameInput
            , onInput NameInputChange
            ]
            []
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
                [ text defaultSelect ]
                :: []
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
                p [] [ text <| location.name ++ " - " ++ location.locationType ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP
-- DECODER
