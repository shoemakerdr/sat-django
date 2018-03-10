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
        , form
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
        , onMouseDown
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
import Filter exposing (Filter)
import Editor
import ToolTip as TT exposing (ToolTip(..))
import Util exposing ((=>))


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


defaultSelect : String
defaultSelect =
    "-- Select type --"


type alias Flags =
    { token : String
    , user : String
    , floorplan :
        { id : Int
        , aspect_ratio : Float
        , image : String
        , is_public : Bool
        , is_trashed : Bool
        , name : String
        , owner : Int
        , owner_name : String
        , locations :
            List
                { id : Int
                , floorplan : Int
                , name : String
                , loc_type : String
                , details : String
                , extension : Maybe Int
                , is_trashed : Bool
                , position_x : Float
                , position_y : Float
                , last_updated : String
                }
        , last_updated : String
        }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { floorplan = newFloorPlan flags.floorplan
    , locations = flags.floorplan.locations
    , nameInput = ""
    , typeSelect = defaultSelect
    , toolTip = Hidden Nothing Nothing
    , filteredLocations = flags.floorplan.locations
    , filters = []
    , floorplanDimensions = Nothing
    , token = flags.token
    , user = flags.user
    , isOwner = flags.user == flags.floorplan.owner_name
    , mode = View flags.floorplan.locations
    }
        ! [ Task.perform ResizeFloorplan Window.size ]



-- MODEL


type alias Model =
    { floorplan : FloorPlan
    , locations : List Location
    , nameInput : String
    , typeSelect : String
    , toolTip : ToolTip Location
    , filteredLocations : List Location
    , filters : List (Filter FilterType Location)
    , floorplanDimensions : Maybe Dimensions
    , token : String
    , user : String
    , isOwner : Bool
    , mode : Mode
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type FilterType
    = Name
    | Type


type Mode
    = View (List Location)
    | Edit (Editor.Editor Location)


type FilterMsg
    = Merge
    | Remove



-- UPDATE


type Msg
    = NameInputChange String
    | TypeSelectChange String
    | ResetFilterForm
    | ShowToolTip (Maybe Mouse.Position) Location
    | HideToolTip
    | ResizeFloorplan Size
    | OpenEditor
    | CancelEditor
    | SaveEditor
    | DoEdit EditMsg


type EditMsg
    = NoOp
    | OpenToolTipEditor (Maybe Mouse.Position) (Maybe Location)
    | CloseToolTipEditor


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
                updateFilter filterMsg (Filter.new Name (filterByName name)) { model | nameInput = name }

        TypeSelectChange locationType ->
            let
                filterMsg =
                    if locationType == defaultSelect then
                        Remove
                    else
                        Merge
            in
                updateFilter filterMsg (Filter.new Type (filterByType locationType)) { model | typeSelect = locationType }

        ResetFilterForm ->
            { model
                | nameInput = ""
                , typeSelect = defaultSelect
                , filteredLocations = model.locations
                , filters = []
            }
                ! []

        ShowToolTip position location ->
            { model | toolTip = Shown position (Just location) } ! []

        HideToolTip ->
            { model | toolTip = Hidden Nothing Nothing } ! []

        ResizeFloorplan size ->
            { model | floorplanDimensions = Just <| getFloorplanDimensions size model.floorplan } ! []

        OpenEditor ->
            { model | mode = Edit (Editor.editor model.locations) } ! []

        CancelEditor ->
            -- need implementation
            { model
                | mode = View model.locations
                , toolTip = Hidden Nothing Nothing
            }
                ! []

        SaveEditor ->
            -- need implementation
            let
                newLocations =
                    saveMode model.mode
            in
                { model
                    | locations = newLocations
                    , mode = View newLocations
                }
                    ! []

        DoEdit editMsg ->
            case editMsg of
                NoOp ->
                    model ! []

                OpenToolTipEditor position location ->
                    case model.toolTip of
                        Hidden _ _ ->
                            { model | toolTip = Shown Nothing location } ! []

                        Shown _ _ ->
                            { model | toolTip = Shown position location } ! []

                CloseToolTipEditor ->
                    { model | toolTip = Hidden Nothing Nothing } ! []


getFloorplanDimensions : Size -> FloorPlan -> Dimensions
getFloorplanDimensions size floorplan =
    let
        width =
            0.7 * (toFloat size.width)
    in
        { width = width
        , height = width * floorplan.aspect_ratio
        }


filterByName : String -> Location -> Bool
filterByName name location =
    String.contains (String.toLower name) (String.toLower location.name)


filterByType : String -> Location -> Bool
filterByType locationType location =
    let
        locType =
            getLocationFromReadable locationType
    in
        locType == location.loc_type


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


saveMode : Mode -> List Location
saveMode mode =
    case mode of
        View locations ->
            locations

        Edit editor ->
            Editor.retrieve editor



-- VIEW


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.map tagger targetValue)


view : Model -> Html Msg
view model =
    let
        toolTipView =
            case model.mode of
                View _ ->
                    viewShowToolTip

                Edit _ ->
                    viewEditToolTip

        config =
            TT.config 10 10 "tooltip-wrapper" []
    in
        div []
            [ h1
                [ class "floorplan-name" ]
                [ text model.floorplan.name ]
            , div [] <|
                if model.isOwner then
                    viewEditorPanel model
                else
                    []
            , div
                [ class "floorplan-main-content" ]
                [ viewFilterLocations model
                , svgMap model
                ]
            , TT.view config toolTipView model.toolTip
            ]


viewEditorPanel : Model -> List (Html Msg)
viewEditorPanel { mode } =
    case mode of
        View _ ->
            [ div []
                [ button [ onClick OpenEditor ] [ text "Edit Floor Plan" ]
                , div [] []
                ]
            ]

        Edit _ ->
            [ div []
                [ button [ onClick SaveEditor ] [ text "Save Changes" ]
                , button [ onClick CancelEditor ] [ text "Cancel" ]
                , div [] []
                ]
            ]


svgMap : Model -> Html Msg
svgMap model =
    case model.floorplanDimensions of
        Nothing ->
            div [] []

        Just dims ->
            div [ class "floorplan-map-wrapper" ]
                [ svg
                    [ width <| toString dims.width
                    , height <| toString dims.height
                    , style
                        [ "background" => ("url(" ++ "http://localhost:8000" ++ model.floorplan.image ++ ")")
                        , "backgroundSize" => "100% auto"
                        , "backgroundRepeat" => "no-repeat"
                        ]
                    ]
                  <|
                    plotLocations dims model
                ]


plotLocations : Dimensions -> Model -> List (Html Msg)
plotLocations dimensions model =
    model.locations
        |> List.map
            (\location ->
                circle
                    ([ SvgAttr.class "location-point"
                     , cx (toString <| location.position_x * dimensions.width)
                     , cy (toString <| location.position_y * dimensions.height)
                     , r "8"
                     , fill "#72acdc"
                     , fillOpacity "0.5"
                     ]
                        ++ locationEvents model.mode location
                    )
                    []
            )


locationEvents : Mode -> Location -> List (Attribute Msg)
locationEvents mode location =
    case mode of
        View _ ->
            [ onMouseEnter (ShowToolTip Nothing location)
            , onMouseLeave HideToolTip
            ]

        Edit _ ->
            [ onClick (DoEdit (OpenToolTipEditor Nothing (Just location)))

            -- , onMouseLeave HideToolTip
            ]


viewShowToolTip : Location -> Html Msg
viewShowToolTip location =
    let
        locationType =
            getLocationFromAbbr location.loc_type
    in
        div []
            [ p []
                [ strong [] [ text "Name: " ]
                , text location.name
                ]
            , p []
                [ strong [] [ text "Ext: " ]
                , text <|
                    case location.extension of
                        Nothing ->
                            ""

                        Just x ->
                            toString x
                ]
            , p []
                [ strong [] [ text "Details: " ]
                , text location.details
                ]
            , p []
                [ strong [] [ text "Type: " ]
                , text locationType
                ]
            ]


viewEditToolTip : Location -> Html Msg
viewEditToolTip location =
    div []
        [ input [ placeholder "Name", value location.name ] []
        , button [ onClick (DoEdit CloseToolTipEditor) ] [ text "Cancel" ]
        ]


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
                let
                    locationType =
                        getLocationFromAbbr location.loc_type
                in
                    p [] [ text <| location.name ++ " - " ++ locationType ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        View _ ->
            case model.toolTip of
                Shown pos location ->
                    case location of
                        Nothing ->
                            Sub.none

                        Just l ->
                            case pos of
                                Nothing ->
                                    Sub.batch [ Mouse.moves (\x -> ShowToolTip (Just x) l) ]

                                Just _ ->
                                    Sub.none

                Hidden _ _ ->
                    Sub.none

        Edit _ ->
            case model.toolTip of
                Shown pos location ->
                    case pos of
                        Nothing ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (OpenToolTipEditor (Just x) location)) ]

                        Just _ ->
                            case location of
                                Nothing ->
                                    Sub.batch [ Mouse.clicks (\x -> DoEdit (CloseToolTipEditor)) ]

                                Just l ->
                                    Sub.batch [ Mouse.clicks (\x -> DoEdit (NoOp)) ]

                Hidden pos location ->
                    case pos of
                        Nothing ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (OpenToolTipEditor (Just x) location)) ]

                        Just _ ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (CloseToolTipEditor)) ]



-- HTTP
-- DECODER
