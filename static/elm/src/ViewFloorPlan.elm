module Main exposing (..)

import Html
    exposing
        ( Html
        , Attribute
        , div
        , text
        , p
        , h1
        , strong
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
import Html.Events
    exposing
        ( onClick
        , onInput
        , onMouseEnter
        , onMouseLeave
        , on
        , targetValue
        )
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
import Mouse exposing (Position)
import Window exposing (Size)
import Task
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
    , floorplanDimensions = Nothing
    }
        ! [ Task.perform ResizeFloorplan Window.size ]



-- MODEL


type alias Model =
    { floorplan : FloorPlan
    , locations : List Location
    , nameInput : String
    , typeSelect : String
    , toolTip : ToolTip
    , filteredLocations : List Location
    , filters : List (Filter FilterType Location)
    , floorplanDimensions : Maybe Dimensions
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type ToolTip
    = Hidden
    | Showing Location (Maybe Mouse.Position)


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
    | ShowToolTip Location (Maybe Mouse.Position)
    | HideToolTip
    | ResizeFloorplan Size


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

        ShowToolTip location position ->
            case position of
                Nothing ->
                    { model | toolTip = Showing location Nothing } ! []

                Just pos ->
                    { model | toolTip = Showing location (Just pos) } ! []

        HideToolTip ->
            { model | toolTip = Hidden } ! []

        ResizeFloorplan size ->
            { model | floorplanDimensions = Just <| getFloorplanDimensions size model.floorplan } ! []


getFloorplanDimensions : Size -> FloorPlan -> Dimensions
getFloorplanDimensions size floorplan =
    let
        width =
            0.7 * (toFloat size.width)
    in
        { width = width
        , height = width * floorplan.heightRatio
        }


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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.map tagger targetValue)


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ class "floorplan-name" ]
            [ text model.floorplan.name ]
        , div
            [ class "floorplan-main-content" ]
            [ viewFilterLocations model
            , svgMap model.floorplan model.floorplanDimensions model.filteredLocations
            ]
        , viewToolTip model.toolTip
        ]


svgMap : FloorPlan -> Maybe Dimensions -> List Location -> Html Msg
svgMap floorplan dimensions locations =
    case dimensions of
        Nothing ->
            div [] []

        Just dims ->
            div [ class "floorplan-map-wrapper" ]
                [ svg
                    [ width <| toString dims.width
                    , height <| toString dims.height
                    , style
                        [ "background" => ("url(" ++ floorplan.src ++ ")")
                        , "backgroundSize" => "100% auto"
                        , "backgroundRepeat" => "no-repeat"
                        ]
                    ]
                  <|
                    plotLocations dims locations
                ]


plotLocations : Dimensions -> List Location -> List (Html Msg)
plotLocations dimensions locations =
    locations
        |> List.map
            (\location ->
                circle
                    [ SvgAttr.class "location-point"
                    , cx (toString <| location.position_x * dimensions.width)
                    , cy (toString <| location.position_y * dimensions.height)
                    , r "8"
                    , fill "#72acdc"
                    , fillOpacity "0.5"
                    , onMouseEnter (ShowToolTip location Nothing)
                    , onMouseLeave HideToolTip
                    ]
                    []
            )


viewToolTip : ToolTip -> Html Msg
viewToolTip toolTip =
    case toolTip of
        Showing location (Just pos) ->
            div
                [ class "tooltip-wrapper"
                , style
                    [ "top" => px (pos.y + 10)
                    , "left" => px (pos.x + 10)
                    ]
                ]
                [ div []
                    [ p []
                        [ strong [] [ text "Name: " ]
                        , text location.name
                        ]
                    , p []
                        [ strong [] [ text "Ext: " ]
                        , text location.extension
                        ]
                    , p []
                        [ strong [] [ text "Details: " ]
                        , text location.details
                        ]
                    , p []
                        [ strong [] [ text "Type: " ]
                        , text location.locationType
                        ]
                    ]
                ]

        _ ->
            div [] []


px : Int -> String
px i =
    toString i ++ "px"


viewFilterLocations : Model -> Html Msg
viewFilterLocations { filteredLocations, nameInput, typeSelect } =
    div [ class "location-filter-wrapper" ]
        [ h1 [ class "location-title" ] [ text "Locations" ]
        , filterForm nameInput typeSelect
        , div [ class "location-list" ] <| locationInfoList filteredLocations
        ]


filterForm : String -> String -> Html Msg
filterForm nameInput typeSelected =
    div []
        [ input
            [ class "form-name-input"
            , placeholder "Filter by name"
            , value nameInput
            , onInput NameInputChange
            ]
            []
        , select [ class "form-select-type", onChange TypeSelectChange ] <| optionList typeSelected
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
    case model.toolTip of
        Showing loc Nothing ->
            Sub.batch [ Mouse.moves (\x -> ShowToolTip loc (Just x)) ]

        _ ->
            Sub.none



-- HTTP
-- DECODER
