port module Main exposing (..)

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
        , id
        , style
        , disabled
        , selected
        , placeholder
        , value
        , type_
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
import Editor exposing (Editor)
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
    , mode = View
    , editor = Editor.editor flags.floorplan.locations
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
    , editor : Editor Location
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type FilterType
    = Name
    | Type


type Mode
    = View
    | Edit EditorAction


type EditorAction
    = WaitingToEdit
    | Add
    | WaitingToMove
    | Move


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
    | SaveToolTipEditor
    | CancelToolTipEditor
    | ReadyToMove
    | CanSetMove
    | GetNewPosition Mouse.Position
    | SetNewPosition ( Float, Float )
    | ChangeName String
    | ChangeType String
    | ChangeDetails String
    | ChangeExtension String


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
            { model | mode = Edit WaitingToEdit } ! []

        CancelEditor ->
            { model
                | mode = View
                , toolTip = Hidden Nothing Nothing
                , editor = Editor.editor model.locations
            }
                ! []

        SaveEditor ->
            let
                newLocations =
                    Editor.retrieve model.editor
            in
                { model
                    | locations = newLocations
                    , filteredLocations = newLocations
                    , toolTip = Hidden Nothing Nothing
                    , mode = View
                    , editor = Editor.editor newLocations
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

                        Shown pos loc ->
                            case ( pos, loc ) of
                                ( Just _, Just _ ) ->
                                    model ! []

                                _ ->
                                    { model
                                        | toolTip = Shown position location
                                        , editor = Editor.maybeEdit location model.editor
                                    }
                                        ! []

                SaveToolTipEditor ->
                    let
                        current =
                            Editor.current model.editor

                        newEditor =
                            case current of
                                Nothing ->
                                    model.editor

                                Just c ->
                                    Editor.update (\l -> l.id == c.id) model.editor
                    in
                        { model
                            | toolTip = Hidden Nothing Nothing
                            , editor = newEditor
                        }
                            ! []

                CancelToolTipEditor ->
                    { model
                        | toolTip = Hidden Nothing Nothing
                        , editor = Editor.cancel model.editor
                    }
                        ! []

                ReadyToMove ->
                    { model
                        | mode = Edit (WaitingToMove)
                        , toolTip = TT.hide model.toolTip
                    }
                        ! [ Task.perform (\_ -> DoEdit CanSetMove) (Task.succeed ()) ]

                CanSetMove ->
                    { model | mode = Edit Move } ! []

                GetNewPosition pos ->
                    model ! [ findCoordinates pos ]

                SetNewPosition ( x, y ) ->
                    let
                        newEditor =
                            case Editor.current model.editor of
                                Nothing ->
                                    model.editor

                                Just location ->
                                    Editor.edit { location | position_x = x, position_y = y } model.editor
                    in
                        { model
                            | editor = newEditor
                            , toolTip = TT.show model.toolTip
                            , mode = Edit WaitingToEdit
                        }
                            ! []

                ChangeName name ->
                    let
                        newEditor =
                            case Editor.current model.editor of
                                Nothing ->
                                    model.editor

                                Just location ->
                                    Editor.edit { location | name = name } model.editor
                    in
                        { model | editor = newEditor } ! []

                ChangeType loc_type ->
                    let
                        newEditor =
                            case Editor.current model.editor of
                                Nothing ->
                                    model.editor

                                Just location ->
                                    Editor.edit { location | loc_type = getLocationFromReadable loc_type } model.editor
                    in
                        { model | editor = newEditor } ! []

                ChangeDetails details ->
                    let
                        newEditor =
                            case Editor.current model.editor of
                                Nothing ->
                                    model.editor

                                Just location ->
                                    Editor.edit { location | details = details } model.editor
                    in
                        { model | editor = newEditor } ! []

                ChangeExtension extension ->
                    let
                        ext =
                            case String.toInt extension of
                                Ok x ->
                                    Just x

                                Err _ ->
                                    Nothing

                        newEditor =
                            case Editor.current model.editor of
                                Nothing ->
                                    model.editor

                                Just location ->
                                    Editor.edit { location | extension = ext } model.editor
                    in
                        { model | editor = newEditor } ! []


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



-- VIEW


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.map tagger targetValue)


view : Model -> Html Msg
view model =
    let
        toolTipView =
            case model.mode of
                View ->
                    viewShowToolTip

                Edit _ ->
                    viewEditToolTip model.editor

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
        View ->
            [ div [ class "editor-panel" ]
                [ button [ onClick OpenEditor ] [ text "Edit Floor Plan" ]
                , div [] []
                ]
            ]

        Edit _ ->
            [ div [ class "editor-panel" ]
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
                    [ id "svg"
                    , width <| toString dims.width
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
    let
        locations =
            case model.mode of
                View ->
                    model.filteredLocations

                Edit _ ->
                    Filter.apply model.filters (Editor.retrieve model.editor)
    in
        locations
            |> removeCurrentEditingLocation model.editor
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
            |> appendCurrentEditing dimensions model


removeCurrentEditingLocation : Editor Location -> List Location -> List Location
removeCurrentEditingLocation editor locations =
    case Editor.current editor of
        Nothing ->
            locations

        Just loc ->
            List.filter (\l -> l.id /= loc.id) locations


appendCurrentEditing : Dimensions -> Model -> List (Html Msg) -> List (Html Msg)
appendCurrentEditing dimensions model locations =
    case Editor.current model.editor of
        Nothing ->
            locations

        Just location ->
            locations
                ++ [ circle
                        ([ SvgAttr.class "editing-location-point"
                         , cx (toString <| location.position_x * dimensions.width)
                         , cy (toString <| location.position_y * dimensions.height)
                         , r "8"
                         , fill "#FF0000"
                         , fillOpacity "0.5"
                         ]
                            ++ locationEvents model.mode location
                        )
                        []
                   ]


locationEvents : Mode -> Location -> List (Attribute Msg)
locationEvents mode location =
    case mode of
        View ->
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


viewEditToolTip : Editor Location -> Location -> Html Msg
viewEditToolTip editor loc =
    let
        location =
            case Editor.current editor of
                Nothing ->
                    loc

                Just l ->
                    l

        extension =
            case Maybe.withDefault (-1) location.extension of
                (-1) ->
                    ""

                ext ->
                    toString ext
    in
        div [ class "tooltip-editor" ]
            [ input [ placeholder "Name", value location.name, onInput (\s -> DoEdit (ChangeName s)) ] []
            , select [ class "form-select-type", onChange (\s -> DoEdit (ChangeType s)) ] <| optionList (getLocationFromAbbr location.loc_type) False
            , input [ placeholder "Details", value location.details, onInput (\s -> DoEdit (ChangeDetails s)) ] []
            , input [ placeholder "Extension", value extension, onInput (\s -> DoEdit (ChangeExtension s)) ] []
            , div [ class "tooltip-editor-buttons" ]
                [ button [ onClick (DoEdit SaveToolTipEditor) ] [ text "Save" ]
                , button [ onClick (DoEdit ReadyToMove) ] [ text "Move" ]
                , button [ onClick (DoEdit CancelToolTipEditor) ] [ text "Cancel" ]
                ]
            ]


viewFilterLocations : Model -> Html Msg
viewFilterLocations model =
    let
        filteredLocations =
            case model.mode of
                View ->
                    model.filteredLocations

                Edit _ ->
                    Filter.apply model.filters (Editor.retrieve model.editor)
    in
        div [ class "location-filter-wrapper" ]
            [ h1 [ class "location-title" ] [ text "Locations" ]
            , filterForm model.nameInput model.typeSelect
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
        , select [ class "form-select-type", onChange TypeSelectChange ] <| optionList typeSelected True
        , button [ onClick ResetFilterForm ] [ text "Reset filter" ]
        ]


optionList : String -> Bool -> List (Html Msg)
optionList typeSelected hasInitialOption =
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
            if hasInitialOption then
                option
                    [ selected <| isTypeSelected typeSelected defaultSelect ]
                    [ text defaultSelect ]
                    :: []
            else
                []
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



-- PORTS


port findCoordinates : Mouse.Position -> Cmd msg


port coordinates : (( Float, Float ) -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        View ->
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

        Edit WaitingToMove ->
            Sub.none

        Edit Move ->
            Sub.batch
                [ Mouse.clicks (\x -> DoEdit (GetNewPosition x))
                , coordinates (\c -> DoEdit (SetNewPosition c))
                ]

        Edit _ ->
            case model.toolTip of
                Shown pos location ->
                    case pos of
                        Nothing ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (OpenToolTipEditor (Just x) location)) ]

                        Just _ ->
                            case location of
                                Nothing ->
                                    Sub.batch [ Mouse.clicks (\x -> DoEdit (CancelToolTipEditor)) ]

                                Just l ->
                                    Sub.batch [ Mouse.clicks (\x -> DoEdit (NoOp)) ]

                Hidden pos location ->
                    case pos of
                        Nothing ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (OpenToolTipEditor (Just x) location)) ]

                        Just _ ->
                            Sub.batch [ Mouse.clicks (\x -> DoEdit (CancelToolTipEditor)) ]



-- HTTP
-- DECODER
