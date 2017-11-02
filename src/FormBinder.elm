module FormBinder
    exposing
        ( initial
        , Model
        , InputModels(..)
        , Msg
        , Msg(..)
        , update
        , textfield
        , datePicker
        , subscriptions
        )

import Html exposing (Html, div, h1, text, p, label, button)
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Error as Error
import Form.Validate as Validate exposing (Validation)
import Ui.Textfield as Textfield
import Ui.DatePicker as DatePicker exposing (DateEvent)
import Date exposing (Date, Day(..), Month(..), day)
import Date.Extra as Date
import Task


type InputModels
    = TextfieldModel Textfield.Model
    | DatePickerModel DatePicker.DatePicker
    | Empty


type alias UiModel =
    Dict String InputModels


type alias Model e output =
    { ui : UiModel
    , form : Form e output
    , date : Maybe Date
    }


getErrorText : Error.ErrorValue e -> String
getErrorText v =
    case v of
        Error.Empty ->
            "Обязательное поле"

        Error.ShorterStringThan len ->
            "Минимальная длина " ++ (toString len) ++ " симв."

        Error.LongerStringThan len ->
            "Максимальная длина " ++ (toString len) ++ " симв."

        Error.InvalidString ->
            "Обязательное поле"

        a ->
            toString a


initial :
    List ( String, Field )
    -> Validation e output
    -> List ( String, InputModels )
    -> ( Model e output, Cmd Msg, DateEvent )
initial initialFields validation initialUi =
    let
        model =
            { ui = Dict.fromList initialUi
            , form = Form.initial initialFields validation
            , date = Just <| Date.fromParts 1992 Feb 21 0 0 0 0
            }
    in
        model ! [ Task.perform CurrentDate Date.now ]


type Msg
    = TextfieldMsg String Textfield.Config Textfield.Msg
    | DatePickerMsg String DatePicker.Msg
    | FormMsg Form.Msg
    | CurrentDate Date


getSubmodel : String -> UiModel -> InputModels
getSubmodel path ui =
    Dict.get path ui |> Maybe.withDefault Empty


getTextfieldModel : UiModel -> String -> Textfield.Model
getTextfieldModel ui path =
    case (getSubmodel path ui) of
        TextfieldModel m ->
            m

        _ ->
            Textfield.defaultModel


getDatepickerModel : UiModel -> String -> DatePicker.DatePicker
getDatepickerModel ui path =
    case (getSubmodel path ui) of
        DatePickerModel m ->
            m

        _ ->
            DatePicker.DatePicker <| DatePicker.defaultModel


newUiModel : UiModel -> String -> InputModels -> UiModel
newUiModel oldModel path subModel =
    let
        upd =
            Dict.update path (\_ -> Just subModel) oldModel
    in
        upd


onlyDatepickers : comparable -> InputModels -> Bool
onlyDatepickers d value =
    case value of
        DatePickerModel _ ->
            True

        _ ->
            False


update : Msg -> Model e output -> Validation e output -> ( Model e output, Cmd Msg, DateEvent )
update msg ({ form } as model) validation =
    case msg of
        CurrentDate today ->
            let
                initDate =
                    (Date.add Date.Day 7 today)

                datepickersKeys =
                    Dict.filter onlyDatepickers model.ui |> Dict.keys

                dfMod =
                    DatePicker.defaultModel

                updater key =
                    Dict.update key (\_ -> Just <| DatePickerModel (DatePicker.DatePicker <| { dfMod | today = today }))

                folded =
                    Dict.foldl
                        (\k v a ->
                            if (List.member k datepickersKeys) then
                                (updater k a)
                            else
                                a
                        )
                        model.ui
            in
                { model
                    | date = Just initDate
                    , ui = folded model.ui
                }
                    ! []

        FormMsg formMsg ->
            { model | form = Form.update validation formMsg form } ! []

        TextfieldMsg fieldName config msg_ ->
            let
                field =
                    Form.getFieldAsString fieldName form

                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        (getTextfieldModel model.ui fieldName)
                        config
                        field.value

                fieldValue =
                    Field.String (newText |> Maybe.withDefault "")

                newFormModel_ =
                    Form.update validation (Form.Input fieldName Form.Text fieldValue) form

                newUi =
                    newUiModel model.ui fieldName (TextfieldModel newTextfieldModel)
            in
                { model | form = newFormModel_, ui = newUi } ! []

        DatePickerMsg fieldName msg ->
            let
                field =
                    Form.getFieldAsString fieldName form

                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update
                        model.date
                        DatePicker.defaultSettings
                        msg
                        (getDatepickerModel model.ui fieldName)

                ( newDate, newFormModel ) =
                    case dateEvent of
                        DatePicker.Changed newDate ->
                            case newDate of
                                Just d ->
                                    let
                                        fv =
                                            Field.String (Date.toIsoString d)

                                        newFormModel_ =
                                            Form.update validation (Form.Input fieldName Form.Text fv) form
                                    in
                                        ( newDate, newFormModel_ )

                                _ ->
                                    ( newDate, form )

                        _ ->
                            ( model.date, form )

                newUi =
                    newUiModel model.ui fieldName (DatePickerModel newDatePicker)
            in
                ( { model
                    | date = newDate
                    , form = newFormModel
                    , ui = newUi
                  }
                , Cmd.map (DatePickerMsg fieldName) datePickerFx
                , dateEvent
                )


textfield : (Msg -> m) -> Model e output -> Textfield.Config -> Html m
textfield lift model config =
    let
        name =
            config.formName |> Maybe.withDefault ""

        hasError field =
            if field.liveError /= Nothing then
                True
            else
                False

        field =
            Form.getFieldAsString name model.form

        config_ =
            { config
                | invalid = hasError field
                , value = field.value
                , errorText =
                    field.liveError
                        |> Maybe.map getErrorText
                        |> Maybe.withDefault ""
            }

        uiModel =
            getTextfieldModel model.ui name
    in
        Textfield.view field.value uiModel config_
            |> Html.map
                (TextfieldMsg name config >> lift)


datePicker : (Msg -> m) -> Model e output -> DatePicker.Settings -> Html m
datePicker lift model settings =
    let
        config =
            settings.textfieldConfig

        name =
            config.formName |> Maybe.withDefault ""

        hasError field =
            if field.liveError /= Nothing then
                True
            else
                False

        field =
            Form.getFieldAsString name model.form

        textfieldConfig =
            { config
                | invalid = hasError field
                , value = field.value
                , required = True
                , errorText =
                    field.liveError
                        |> Maybe.map getErrorText
                        |> Maybe.withDefault ""
            }

        uiModel =
            getDatepickerModel model.ui name

        datePickerConfig =
            { settings
                | textfieldConfig = textfieldConfig
            }

        date =
            Date.fromIsoString (field.value |> Maybe.withDefault "")
    in
        DatePicker.view date datePickerConfig uiModel |> Html.map (DatePickerMsg name >> lift)


subscriptions : Model e o -> Sub Msg
subscriptions { ui } =
    let
        datepickersKeys =
            Dict.filter onlyDatepickers ui |> Dict.keys

        getter key dict =
            case Dict.get key dict of
                Nothing ->
                    DatePicker.DatePicker DatePicker.defaultModel

                Just v ->
                    case v of
                        DatePickerModel m ->
                            m

                        _ ->
                            DatePicker.DatePicker DatePicker.defaultModel

        subs =
            List.map (\k -> Sub.map (DatePickerMsg k) (DatePicker.subscriptions (getter k ui))) datepickersKeys
    in
        Sub.batch subs


(!) : Model e o -> List (Cmd Msg) -> ( Model e o, Cmd Msg, DateEvent )
(!) m cs =
    ( m, Cmd.batch cs, DatePicker.NoChange )
