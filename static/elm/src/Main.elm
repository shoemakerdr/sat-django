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
        )
import Svg exposing (Svg, svg, circle)
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
import Keyboard
import Window exposing (Size)
import Task
import Json.Decode as Json
import Data.FloorPlan as FloorPlan exposing (FloorPlan)
import Data.Location as Location exposing (Location)
import Data.Id as Id exposing (Id)
import Data.Filter as Filter exposing (Filter)
import Data.Editor as Editor exposing (Editor)
import ToolTip as TT exposing (ToolTip(..))
import Util exposing ((=>), onChange, onClickWithPosition, (@))


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


isTrashedFilter : Filter FilterType Location
isTrashedFilter =
    (Filter.new IsTrashed (not << .is_trashed))


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
        , locations : Json.Value
        , last_updated : String
        }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        locations =
            Location.decodeLocations flags.floorplan.locations
    in
        { floorplan = FloorPlan.floorplan flags.floorplan
        , locations = locations
        , floorplanNameInput = flags.floorplan.name
        , filterNameInput = ""
        , filterTypeSelect = defaultSelect
        , toolTip = Hidden Nothing
        , filters = [ isTrashedFilter ]
        , floorplanDimensions = Nothing
        , token = flags.token
        , user = flags.user
        , isOwner = flags.user == flags.floorplan.owner_name
        , mode = View
        , editor = Editor.editor locations
        }
            ! [ Task.perform ResizeFloorplan Window.size ]



-- MODEL


type alias Model =
    { floorplan : FloorPlan
    , locations : List Location
    , floorplanNameInput : String
    , filterNameInput : String
    , filterTypeSelect : String
    , toolTip : ToolTip
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
    | IsTrashed


type Mode
    = View
    | Edit Status


type Status
    = Waiting
    | Adding
    | FloorPlanName
    | Editing
    | PreparingForMove
    | Moving
    | DeleteConfirmation
    | InvalidLocation


type FilterMsg
    = Merge
    | Remove



-- UPDATE


type Msg
    = NameInputChange String
    | TypeSelectChange String
    | ResetFilterForm
    | ShowLocationInfo Location
    | ShowToolTip (Maybe Position)
    | HideToolTip
    | ResizeFloorplan Size
    | BeginEdit
    | CancelEdit
    | SaveEdit
    | DoEdit EditMsg


type EditMsg
    = NoOp
    | EditFloorPlanName
    | ChangeFloorPlanName String
    | SaveFloorPlanName
    | CancelFloorPlanName
    | OpenToolTipEditor (Maybe Location) Position
    | SaveToolTipEditor
    | CancelToolTipEditor
    | ReadyToMove
    | CanSetMove
    | GetNewPosition Position
    | SetNewPosition ( Float, Float )
    | ChangeName String
    | ChangeType String
    | ChangeDetails String
    | ChangeExtension String
    | ReadyToDelete
    | DeleteLocation
    | CancelDelete


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
                updateFilter filterMsg (Filter.new Name (filterByName name)) { model | filterNameInput = name }

        TypeSelectChange locationType ->
            let
                filterMsg =
                    if locationType == defaultSelect then
                        Remove
                    else
                        Merge
            in
                updateFilter filterMsg (Filter.new Type (filterByType locationType)) { model | filterTypeSelect = locationType }

        ResetFilterForm ->
            { model
                | filterNameInput = ""
                , filterTypeSelect = defaultSelect
                , filters = [ isTrashedFilter ]
            }
                ! []

        ShowLocationInfo location ->
            let
                newEditor =
                    Editor.edit location model.editor
            in
                { model
                    | editor = newEditor
                    , toolTip = Shown Nothing
                }
                    ! []

        ShowToolTip position ->
            let
                newMode =
                    case model.mode of
                        View ->
                            model.mode

                        Edit _ ->
                            Edit Editing
            in
                { model
                    | toolTip = Shown position
                    , mode = newMode
                }
                    ! []

        HideToolTip ->
            { model
                | toolTip = Hidden Nothing
                , editor = Editor.cancel model.editor
            }
                ! []

        ResizeFloorplan size ->
            { model | floorplanDimensions = Just <| getFloorplanDimensions size model.floorplan } ! []

        BeginEdit ->
            { model | mode = Edit Waiting } ! []

        CancelEdit ->
            { model
                | mode = View
                , toolTip = Hidden Nothing
                , editor = Editor.editor model.locations
            }
                ! []

        SaveEdit ->
            let
                newLocations =
                    Editor.list model.editor

                { floorplan } =
                    model

                newFloorplan =
                    { floorplan | name = model.floorplanNameInput }
            in
                { model
                    | locations = newLocations
                    , floorplan = newFloorplan
                    , toolTip = Hidden Nothing
                    , mode = View
                    , editor = Editor.editor newLocations
                }
                    ! []

        DoEdit editMsg ->
            updateEditor editMsg model


updateEditor : EditMsg -> Model -> ( Model, Cmd Msg )
updateEditor editMsg model =
    case editMsg of
        NoOp ->
            model ! []

        EditFloorPlanName ->
            { model | mode = Edit FloorPlanName } ! []

        ChangeFloorPlanName name ->
            { model | floorplanNameInput = name } ! []

        SaveFloorPlanName ->
            let
                { floorplan } =
                    model
            in
                { model
                    | mode = Edit Waiting

                    -- , floorplan = { floorplan | name = model.floorplanNameInput }
                }
                    ! []

        CancelFloorPlanName ->
            { model | mode = Edit Waiting } ! []

        OpenToolTipEditor location position ->
            case Editor.current model.editor of
                Nothing ->
                    let
                        id =
                            Id.nextId <| Editor.list model.editor

                        loc =
                            Maybe.withDefault (Location.new id model.floorplan.id) location

                        newToolTip =
                            case location of
                                Nothing ->
                                    Hidden (Just position)

                                Just _ ->
                                    Shown (Just position)

                        cmd =
                            case location of
                                Nothing ->
                                    findCoordinates position

                                Just _ ->
                                    Cmd.none

                        newMode =
                            case location of
                                Nothing ->
                                    Edit Adding

                                Just _ ->
                                    Edit Editing
                    in
                        { model
                            | editor = Editor.edit loc model.editor
                            , mode = newMode
                            , toolTip = newToolTip
                        }
                            ! [ cmd ]

                Just _ ->
                    model ! []

        SaveToolTipEditor ->
            validateLocationOnSave model

        CancelToolTipEditor ->
            { model
                | toolTip = Hidden Nothing
                , editor = Editor.cancel model.editor
                , mode = Edit Waiting
            }
                ! []

        ReadyToMove ->
            { model
                | mode = Edit PreparingForMove
                , toolTip = TT.hide model.toolTip
            }
                ! [ Task.perform (\_ -> DoEdit CanSetMove) (Task.succeed ()) ]

        CanSetMove ->
            { model | mode = Edit Moving } ! []

        GetNewPosition pos ->
            { model | toolTip = TT.move pos model.toolTip } ! [ findCoordinates pos ]

        SetNewPosition ( x, y ) ->
            let
                newEditor =
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | position_x = x, position_y = y }
                        )
                        model.editor
            in
                { model
                    | editor = newEditor
                    , toolTip = TT.show model.toolTip
                    , mode = Edit Editing
                }
                    ! []

        ChangeName name ->
            let
                newEditor =
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | name = name }
                        )
                        model.editor
            in
                { model | editor = newEditor } ! []

        ChangeType loc_type ->
            let
                newEditor =
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | loc_type = Location.fromReadable loc_type }
                        )
                        model.editor
            in
                { model | editor = newEditor } ! []

        ChangeDetails details ->
            let
                newEditor =
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | details = details }
                        )
                        model.editor
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
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | extension = ext }
                        )
                        model.editor
            in
                { model | editor = newEditor } ! []

        ReadyToDelete ->
            { model | mode = Edit DeleteConfirmation } ! []

        DeleteLocation ->
            let
                editor_ =
                    Editor.newWithDefault
                        (\location ->
                            Editor.edit { location | is_trashed = True }
                        )
                        model.editor

                newEditor =
                    Editor.newWithDefault
                        (\c ->
                            Editor.update (Location.equal c)
                        )
                        editor_
            in
                { model
                    | mode = Edit Waiting
                    , editor = newEditor
                    , toolTip = Hidden Nothing
                }
                    ! []

        CancelDelete ->
            { model | mode = Edit Waiting } ! []


validateLocationOnSave : Model -> ( Model, Cmd Msg )
validateLocationOnSave model =
    case Editor.current model.editor of
        Nothing ->
            model ! []

        Just c ->
            if Location.isValid c then
                let
                    newEditor =
                        if Editor.isSaved Location.equal model.editor then
                            Editor.create model.editor
                        else
                            Editor.newWithDefault (\c -> Editor.update (Location.equal c)) model.editor
                in
                    { model
                        | toolTip = Hidden Nothing
                        , editor = newEditor
                        , mode = Edit Waiting
                    }
                        ! []
            else
                { model | mode = Edit InvalidLocation } ! []


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
            Location.fromReadable locationType
    in
        locType == location.loc_type


updateFilter : FilterMsg -> Filter FilterType Location -> Model -> ( Model, Cmd Msg )
updateFilter filterMsg filter model =
    { model
        | filters =
            case filterMsg of
                Remove ->
                    Filter.remove filter model.filters

                Merge ->
                    Filter.merge filter model.filters
    }
        ! []



-- VIEW


view : Model -> Html Msg
view model =
    let
        toolTipView =
            case Editor.current model.editor of
                Nothing ->
                    div [] []

                Just location ->
                    case model.mode of
                        View ->
                            viewShowToolTip location

                        Edit DeleteConfirmation ->
                            viewDeleteToolTip location

                        Edit InvalidLocation ->
                            viewValidationToolTip location

                        Edit _ ->
                            viewEditToolTip location

        config =
            TT.config 10 10 "tooltip-wrapper" []
    in
        div []
            [ editableFloorPlanName model
            , div [] <|
                if model.isOwner then
                    viewEditorPanel model
                else
                    []
            , div
                [ class "floorplan-main-content" ]
                [ viewFilterPanel model
                , svgMap model
                ]
            , TT.view config (toolTipView) model.toolTip
            ]


editableFloorPlanName : Model -> Html Msg
editableFloorPlanName model =
    case model.mode of
        Edit FloorPlanName ->
            input [ class "floorplan-name-editing", onInput (\name -> DoEdit (ChangeFloorPlanName name)), value model.floorplanNameInput ] []

        Edit _ ->
            h1
                [ class "floorplan-name", onClick (DoEdit EditFloorPlanName) ]
                [ text model.floorplanNameInput ]

        _ ->
            h1
                [ class "floorplan-name" ]
                [ text model.floorplan.name ]


viewEditorPanel : Model -> List (Html Msg)
viewEditorPanel { mode } =
    case mode of
        View ->
            [ div [ class "editor-panel" ]
                [ button [ onClick BeginEdit ] [ text "Edit Floor Plan" ]
                , div [] []
                ]
            ]

        Edit _ ->
            [ div [ class "editor-panel" ]
                [ button [ onClick SaveEdit ] [ text "Save Changes" ]
                , button [ onClick CancelEdit ] [ text "Cancel" ]
                , div [] []
                ]
            ]


svgMap : Model -> Html Msg
svgMap model =
    let
        clickEvents =
            case model.mode of
                Edit Waiting ->
                    [ onClickWithPosition (\position -> DoEdit (OpenToolTipEditor Nothing position)) ]

                _ ->
                    []
    in
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
                            ++ clickEvents
                        )
                      <|
                        plotLocations dims model
                    ]


plotLocations : Dimensions -> Model -> List (Html Msg)
plotLocations dimensions model =
    let
        filteredLocations =
            Filter.apply model.filters (Editor.list model.editor)

        locations =
            case model.mode of
                View ->
                    filteredLocations

                Edit _ ->
                    case Editor.current model.editor of
                        Nothing ->
                            filteredLocations

                        Just l ->
                            List.filter (not << (Location.equal l)) filteredLocations

        current =
            case model.mode of
                View ->
                    [ circle [ SvgAttr.visibility "collapse" ] [] ]

                Edit _ ->
                    Editor.current model.editor
                        |> Maybe.map
                            (\location ->
                                [ viewCircle "#FF0000" "editing-location-point" model.mode dimensions location ]
                            )
                        |> Maybe.withDefault []
    in
        locations
            |> List.map
                (\location ->
                    viewCircle "#72acdc" "location-point" model.mode dimensions location
                )
            |> (++) current


viewCircle : String -> String -> Mode -> Dimensions -> Location -> Svg Msg
viewCircle color className mode dimensions location =
    circle
        ([ SvgAttr.class className
         , cx (toString <| location.position_x * dimensions.width)
         , cy (toString <| location.position_y * dimensions.height)
         , r "8"
         , fill color
         , fillOpacity "0.5"
         ]
            ++ locationEvents mode location
        )
        []


locationEvents : Mode -> Location -> List (Attribute Msg)
locationEvents mode location =
    case mode of
        View ->
            [ onMouseEnter (ShowLocationInfo location)
            , onMouseLeave HideToolTip
            ]

        Edit _ ->
            [ onClickWithPosition (\position -> DoEdit (OpenToolTipEditor (Just location) position)) ]


viewShowToolTip : Location -> Html Msg
viewShowToolTip location =
    let
        locationType =
            Location.fromAbbr location.loc_type
    in
        div []
            [ p []
                [ strong [] [ text "Name: " ]
                , text location.name
                ]
            , p []
                [ strong [] [ text "Ext: " ]
                , text <| Location.extensionToString location.extension
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
    let
        extension =
            Location.extensionToString location.extension
    in
        div [ class "tooltip-editor" ]
            [ input [ placeholder "Name", value location.name, onInput (\s -> DoEdit (ChangeName s)) ] []
            , select [ class "form-select-type", onChange (\s -> DoEdit (ChangeType s)) ] <| optionList (Location.fromAbbr location.loc_type) False
            , input [ placeholder "Details", value location.details, onInput (\s -> DoEdit (ChangeDetails s)) ] []
            , input [ placeholder "Extension", value extension, onInput (\s -> DoEdit (ChangeExtension s)) ] []
            , div [ class "tooltip-editor-buttons" ]
                [ button [ onClick (DoEdit SaveToolTipEditor) ] [ text "Save" ]
                , button [ onClick (DoEdit ReadyToMove) ] [ text "Move" ]
                , button [ class "delete-button", onClick (DoEdit ReadyToDelete) ] [ text "Delete" ]
                , button [ onClick (DoEdit CancelToolTipEditor) ] [ text "Cancel" ]
                ]
            ]


viewDeleteToolTip : Location -> Html Msg
viewDeleteToolTip location =
    div []
        [ viewEditToolTip location
        , div []
            [ p [ class "delete-warning" ] [ text ("Are you sure you want to delete " ++ location.name ++ "?") ]
            , div [ class "tooltip-editor-buttons" ]
                [ button [ class "delete-button", onClick (DoEdit DeleteLocation) ] [ text "Yes" ]
                , button [ onClick (DoEdit CancelDelete) ] [ text "Cancel" ]
                ]
            ]
        ]


viewValidationToolTip : Location -> Html Msg
viewValidationToolTip location =
    div []
        [ viewEditToolTip location
        , div [ class "validation-error" ] [ text "Location must be have a name." ]
        ]


viewFilterPanel : Model -> Html Msg
viewFilterPanel model =
    let
        locations =
            Filter.apply model.filters (Editor.list model.editor)
    in
        div [ class "location-filter-wrapper" ]
            [ h1 [ class "location-title" ] [ text "Locations" ]
            , filterForm model.filterNameInput model.filterTypeSelect
            , div [ class "location-list" ] <| locationInfoList locations
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
                        Location.fromAbbr location.loc_type

                    extension =
                        Location.extensionToString location.extension

                    extString =
                        if extension == "" then
                            ""
                        else
                            ", ext. " ++ extension
                in
                    p [ class "location-list-info" ] [ text <| location.name ++ " - " ++ locationType ++ extString ]
            )



-- PORTS


port findCoordinates : Position -> Cmd msg


port coordinates : (( Float, Float ) -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        View ->
            case model.toolTip of
                Shown pos ->
                    case pos of
                        Nothing ->
                            Sub.batch [ Mouse.moves (\x -> ShowToolTip (Just x)) ]

                        Just _ ->
                            Sub.none

                Hidden _ ->
                    Sub.none

        Edit Moving ->
            Sub.batch
                [ Mouse.clicks (\x -> DoEdit (GetNewPosition x))
                , coordinates (\c -> DoEdit (SetNewPosition c))
                ]

        Edit FloorPlanName ->
            Sub.batch
                [ Keyboard.ups
                    (\keyCode ->
                        case keyCode of
                            13 ->
                                DoEdit SaveFloorPlanName

                            27 ->
                                DoEdit CancelFloorPlanName

                            _ ->
                                DoEdit NoOp
                    )
                ]

        Edit _ ->
            Sub.batch
                [ coordinates (\c -> DoEdit (SetNewPosition c))
                ]
