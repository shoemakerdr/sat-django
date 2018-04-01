port module Main exposing (..)

import Data.Dimensions as Dimensions exposing (Dimensions)
import Data.FloorPlan as FloorPlan exposing (FloorPlan)
import Data.Location as Location exposing (Location)
import Data.Id as Id exposing (Id)
import Data.Editor.List as LEditor
import Data.Editor.String as SEditor
import Data.Mode exposing (..)
import Html exposing (Html, Attribute, div, text, p, h1, strong, button, input, select)
import Html.Attributes exposing (src, class, id, style, placeholder, value, type_)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseEnter, onMouseLeave)
import Json.Decode as JD
import Keyboard
import Mouse exposing (Position)
import Task
import Util exposing ((=>), onChange, onClickWithPosition, (@))
import View.FilterPanel as FilterPanel
import View.Options as Options
import View.SvgMap as SvgMap
import View.ToolTip as TT exposing (ToolTip(..))
import Window exposing (Size)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
        , locations : JD.Value
        , last_updated : String
        }
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        locations =
            case Location.decodeLocations flags.floorplan.locations of
                Ok l ->
                    l

                -- TODO: Handle this error case
                Err _ ->
                    []
    in
        { floorplan = FloorPlan.floorplan flags.floorplan
        , locations = locations
        , filterPanel = FilterPanel.initialModel
        , toolTip = Hidden Nothing
        , floorplanDimensions = Nothing
        , token = flags.token
        , user = flags.user
        , isOwner = flags.user == flags.floorplan.owner_name
        , mode = View
        , locationEditor = LEditor.editor locations
        , floorplanEditor = SEditor.editor flags.floorplan.name
        }
            ! [ Task.perform ResizeFloorplan Window.size ]



-- MODEL


type alias Model =
    { floorplan : FloorPlan
    , locations : List Location
    , toolTip : ToolTip
    , filterPanel : FilterPanel.Model
    , floorplanDimensions : Maybe Dimensions
    , token : String
    , user : String
    , isOwner : Bool
    , mode : Mode
    , locationEditor : LEditor.Editor Location
    , floorplanEditor : SEditor.Editor
    }



-- UPDATE


type Msg
    = NoOp
    | FilterPanelMsg FilterPanel.Msg
    | ShowLocationInfo Location
    | ShowToolTip (Maybe Position)
    | HideToolTip
    | ResizeFloorplan Size
    | BeginEdit
    | CancelEdit
    | SaveEdit
    | EditFloorPlanName
    | SaveFloorPlanName
    | CancelFloorPlanName
    | FloorPlanNameChange String
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
        NoOp ->
            model ! []

        FilterPanelMsg panelMsg ->
            let
                { filterPanel } =
                    model
            in
                { model
                    | filterPanel = FilterPanel.update panelMsg filterPanel
                }
                    ! []

        ShowLocationInfo location ->
            let
                newEditor =
                    LEditor.edit location model.locationEditor
            in
                { model
                    | locationEditor = newEditor
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
                , locationEditor = LEditor.cancel model.locationEditor
            }
                ! []

        ResizeFloorplan size ->
            { model | floorplanDimensions = Just <| Dimensions.get 0.7 size model.floorplan.aspect_ratio } ! []

        BeginEdit ->
            { model | mode = Edit Waiting } ! []

        CancelEdit ->
            { model
                | mode = View
                , toolTip = Hidden Nothing
                , locationEditor = LEditor.editor model.locations
                , floorplanEditor = SEditor.editor model.floorplan.name
            }
                ! []

        SaveEdit ->
            let
                newLocations =
                    LEditor.list model.locationEditor

                { floorplan } =
                    model

                newFloorplan =
                    { floorplan | name = SEditor.saved model.floorplanEditor }
            in
                { model
                    | locations = newLocations
                    , floorplan = newFloorplan
                    , toolTip = Hidden Nothing
                    , mode = View
                    , locationEditor = LEditor.editor newLocations
                }
                    ! []

        EditFloorPlanName ->
            { model | mode = Edit FloorPlanName } ! []

        FloorPlanNameChange name ->
            { model | floorplanEditor = SEditor.edit name model.floorplanEditor } ! []

        SaveFloorPlanName ->
            { model
                | mode = Edit Waiting
                , floorplanEditor = SEditor.update model.floorplanEditor
            }
                ! []

        CancelFloorPlanName ->
            { model
                | mode = Edit Waiting
                , floorplanEditor = SEditor.cancel model.floorplanEditor
            }
                ! []

        OpenToolTipEditor location position ->
            case LEditor.current model.locationEditor of
                Nothing ->
                    let
                        id =
                            Id.nextId <| LEditor.list model.locationEditor

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
                            | locationEditor = LEditor.edit loc model.locationEditor
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
                , locationEditor = LEditor.cancel model.locationEditor
                , mode = Edit Waiting
            }
                ! []

        ReadyToMove ->
            { model
                | mode = Edit PreparingForMove
                , toolTip = TT.hide model.toolTip
            }
                ! [ Task.perform (\_ -> CanSetMove) (Task.succeed ()) ]

        CanSetMove ->
            { model | mode = Edit Moving } ! []

        GetNewPosition pos ->
            { model | toolTip = TT.move pos model.toolTip } ! [ findCoordinates pos ]

        SetNewPosition ( x, y ) ->
            let
                newEditor =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | position_x = x, position_y = y }
                        )
                        model.locationEditor
            in
                { model
                    | locationEditor = newEditor
                    , toolTip = TT.show model.toolTip
                    , mode = Edit Editing
                }
                    ! []

        ChangeName name ->
            let
                newEditor =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | name = name }
                        )
                        model.locationEditor
            in
                { model | locationEditor = newEditor } ! []

        ChangeType loc_type ->
            let
                newEditor =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | loc_type = Location.fromReadable loc_type }
                        )
                        model.locationEditor
            in
                { model | locationEditor = newEditor } ! []

        ChangeDetails details ->
            let
                newEditor =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | details = details }
                        )
                        model.locationEditor
            in
                { model | locationEditor = newEditor } ! []

        ChangeExtension extension ->
            let
                ext =
                    case extension of
                        "" ->
                            Nothing

                        _ ->
                            case String.toInt extension of
                                Ok x ->
                                    Just x

                                Err _ ->
                                    case LEditor.current model.locationEditor of
                                        Nothing ->
                                            Nothing

                                        Just location ->
                                            location.extension

                newEditor =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | extension = ext }
                        )
                        model.locationEditor
            in
                { model | locationEditor = newEditor } ! []

        ReadyToDelete ->
            { model | mode = Edit DeleteConfirmation } ! []

        DeleteLocation ->
            let
                editor_ =
                    LEditor.newWithDefault
                        (\location ->
                            LEditor.edit { location | is_trashed = True }
                        )
                        model.locationEditor

                newEditor =
                    LEditor.newWithDefault
                        (\c ->
                            LEditor.update (Location.equal c)
                        )
                        editor_
            in
                { model
                    | mode = Edit Waiting
                    , locationEditor = newEditor
                    , toolTip = Hidden Nothing
                }
                    ! []

        CancelDelete ->
            { model | mode = Edit Waiting } ! []


validateLocationOnSave : Model -> ( Model, Cmd Msg )
validateLocationOnSave model =
    case LEditor.current model.locationEditor of
        Nothing ->
            model ! []

        Just c ->
            if Location.isValid c then
                let
                    newEditor =
                        if LEditor.isSaved Location.equal model.locationEditor then
                            LEditor.create model.locationEditor
                        else
                            LEditor.newWithDefault (\c -> LEditor.update (Location.equal c)) model.locationEditor
                in
                    { model
                        | toolTip = Hidden Nothing
                        , locationEditor = newEditor
                        , mode = Edit Waiting
                    }
                        ! []
            else
                { model | mode = Edit InvalidLocation } ! []



-- VIEW


svgMapEvents : Mode -> { mapEvents : List (Attribute Msg), locationEvents : Location -> List (Attribute Msg) }
svgMapEvents mode =
    { mapEvents = mapEvents mode
    , locationEvents = locationEvents mode
    }


mapEvents : Mode -> List (Attribute Msg)
mapEvents mode =
    case mode of
        Edit Waiting ->
            [ onClickWithPosition (\position -> OpenToolTipEditor Nothing position) ]

        _ ->
            []


locationEvents : Mode -> Location -> List (Attribute Msg)
locationEvents mode =
    \location ->
        case mode of
            View ->
                [ onMouseEnter (ShowLocationInfo location)
                , onMouseLeave HideToolTip
                ]

            Edit _ ->
                [ onClickWithPosition (\position -> OpenToolTipEditor (Just location) position) ]


view : Model -> Html Msg
view model =
    let
        toolTipView =
            case LEditor.current model.locationEditor of
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
                [ FilterPanel.view model.locations model.filterPanel
                    |> Html.map FilterPanelMsg
                , SvgMap.view (svgMapEvents model.mode) model
                ]
            , TT.view config (toolTipView) model.toolTip
            ]


editableFloorPlanName : Model -> Html Msg
editableFloorPlanName model =
    case model.mode of
        Edit FloorPlanName ->
            input [ class "floorplan-name-editing", onInput (\name -> FloorPlanNameChange name), value (SEditor.current model.floorplanEditor) ] []

        Edit _ ->
            h1
                [ class "floorplan-name-editable", onClick EditFloorPlanName ]
                [ text (SEditor.saved model.floorplanEditor) ]

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
            [ input [ placeholder "Name", value location.name, onInput (\s -> ChangeName s) ] []
            , select [ class "form-select-type", onChange (\s -> ChangeType s) ] <| Options.view (Location.fromAbbr location.loc_type) False
            , input [ placeholder "Details", value location.details, onInput (\s -> ChangeDetails s) ] []
            , input [ placeholder "Extension", value extension, onInput (\s -> ChangeExtension s) ] []
            , div [ class "tooltip-editor-buttons" ]
                [ button [ onClick SaveToolTipEditor ] [ text "Save" ]
                , button [ onClick ReadyToMove ] [ text "Move" ]
                , button [ class "delete-button", onClick ReadyToDelete ] [ text "Delete" ]
                , button [ onClick CancelToolTipEditor ] [ text "Cancel" ]
                ]
            ]


viewDeleteToolTip : Location -> Html Msg
viewDeleteToolTip location =
    div []
        [ viewEditToolTip location
        , div []
            [ p [ class "delete-warning" ] [ text ("Are you sure you want to delete " ++ location.name ++ "?") ]
            , div [ class "tooltip-editor-buttons" ]
                [ button [ class "delete-button", onClick DeleteLocation ] [ text "Yes" ]
                , button [ onClick CancelDelete ] [ text "Cancel" ]
                ]
            ]
        ]


viewValidationToolTip : Location -> Html Msg
viewValidationToolTip location =
    div []
        [ viewEditToolTip location
        , div [ class "validation-error" ] [ text "Location must be have a name." ]
        ]



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
                [ Mouse.clicks (\x -> GetNewPosition x)
                , coordinates (\c -> SetNewPosition c)
                ]

        Edit FloorPlanName ->
            Sub.batch
                [ Keyboard.ups
                    (\keyCode ->
                        case keyCode of
                            13 ->
                                SaveFloorPlanName

                            27 ->
                                CancelFloorPlanName

                            _ ->
                                NoOp
                    )
                ]

        Edit _ ->
            Sub.batch
                [ coordinates (\c -> SetNewPosition c)
                ]
