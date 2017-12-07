port module Ui.ImageTile exposing (..)

import Html exposing (Html, div, input, label, text, img, span)
import Html.Attributes exposing (type_, id, src, for, style, accept)
import Html.Events exposing (on, onClick, onWithOptions)
import Json.Decode as JD
import Ui.Options as Options exposing (styled, cs, css, when)
import Icons.Icon as Icon
import Ui.Typography as Typography
import Html.Attributes as Attrs


type alias InputId =
    String


type alias FileRecordMetaData =
    { comment : Maybe String
    , linkRef : Maybe String
    , fileName : String
    }


type alias FileRecord =
    { url : String
    , id : String
    , meta : FileRecordMetaData
    , preview : Maybe String
    }


type Msg
    = Clear
    | ImageSelected InputId
    | OpenImage FileRecord
    | FileRendered RenderedFile
    | PreviewRendered RenderedPreview


type alias RenderedFile =
    { fileName : String
    , content : String
    , inputId : String
    }


type alias RenderedPreview =
    { content : String
    , inputId : String
    }


type alias Model =
    { file : Maybe FileRecord
    }


defaultModel : Model
defaultModel =
    { file = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.none
    )


extractIdFromMsg : Msg -> String
extractIdFromMsg msg =
    Debug.log "inputId" <|
        case msg of
            PreviewRendered file ->
                file.inputId

            FileRendered file ->
                file.inputId

            ImageSelected inputId ->
                inputId

            _ ->
                ""


renderImage : FileRecord -> Html Msg
renderImage file =
    let
        filename_ =
            file.meta.fileName

        url_ =
            Maybe.withDefault "" file.preview

        thumbStyle =
            [ ( "width", "168px" )
            , ( "height", "120px" )
            , ( "position", "relative" )
            , ( "background", "#edeff1" )
            , ( "flex", "1 0 auto" )
            , ( "display", "inline-flex" )
            , ( "margin-right", "24px" )
            , ( "margin-bottom", "24px" )
            , ( "background-repeat", "no-repeat" )
            , ( "background-position-x", "center" )
            , ( "background-position-y", "center" )
            , ( "background-size", "cover" )
            , ( "background-image", "url(" ++ url_ ++ ")" )
            ]

        thumbInfoStyle =
            [ ( "width", "168px" )
            , ( "height", "48px" )
            , ( "background-color", "#9b9b9b" )
            , ( "position", "absolute" )
            , ( "bottom", "0" )
            , ( "padding", "12px" )
            , ( "box-sizing", "border-box" )
            , ( "opacity", "0.9" )
            ]

        titleStyle =
            [ ( "font-size", "16px" )
            , ( "text-align", "left" )
            , ( "color", "#ffffff" )
            , ( "font-weight", "300" )
            , ( "text-overflow", "ellipsis" )
            , ( "overflow", "hidden" )
            , ( "white-space", "nowrap" )
            , ( "display", "block" )
            , ( "width", "120px" )
            ]

        btnStyles =
            [ ( "position", "absolute" )
            , ( "right", "12px" )
            , ( "top", "10px" )
            ]

        deleteIconStyle =
            [ ( "color", "#cfd8dc" )
            ]

        thumb =
            div
                [ style thumbStyle
                , onClick (OpenImage file)
                ]
                [ div
                    [ style thumbInfoStyle ]
                    [ span [ style titleStyle ] [ text filename_ ]
                    , div [ style btnStyles ]
                        [ Icon.asButton "delete"
                            [ onWithOptions "click"
                                { stopPropagation = True
                                , preventDefault = False
                                }
                                (JD.succeed Clear)
                            ]
                            [ style deleteIconStyle ]
                        ]
                    ]
                ]
    in
        div [] [ thumb ]


renderPlaceholder : InputId -> Html Msg
renderPlaceholder inputId =
    let
        placeholderStyle =
            [ ( "background-color", "#EDEFF1" )
            , ( "width", "168px" )
            , ( "height", "120px" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "center" )
            ]
    in
        div []
            [ label
                [ style placeholderStyle
                , for inputId
                ]
                [ Icon.view "add" [] ]
            , input
                [ type_ "file"
                , on "change" (JD.succeed (ImageSelected inputId))
                , id inputId
                , accept ".pdf"
                , style
                    [ ( "display", "none" )
                    ]
                ]
                []
            ]


view : InputId -> Model -> Html Msg
view inputId model =
    let
        body =
            case model.file of
                Just file ->
                    renderImage file

                Nothing ->
                    renderPlaceholder inputId
    in
        div []
            [ div []
                [ styled div
                    [ Typography.headline, Typography.pad24 ]
                    [ text "Сканы паспорта *" ]
                ]
            , body
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageSelected inputId ->
            ( model
            , imageSelected inputId
            )

        FileRendered renderedFile ->
            let
                meta =
                    { fileName = renderedFile.fileName
                    , comment = Nothing
                    , linkRef = Nothing
                    }

                newFile =
                    { url = renderedFile.content
                    , preview = Nothing
                    , meta = meta
                    , id = ""
                    }
            in
                ( { model | file = Just newFile }
                , Cmd.none
                )

        PreviewRendered renderedPreview ->
            let
                newFile =
                    Maybe.map (\t -> { t | preview = Just renderedPreview.content }) model.file
            in
                ( { model | file = newFile }
                , Cmd.none
                )

        OpenImage file ->
            ( model, openImage file )

        Clear ->
            ( { model | file = Nothing }
            , Cmd.none
            )


port imageSelected : String -> Cmd msg


port openImage : FileRecord -> Cmd msg


port fileRendered : (RenderedFile -> msg) -> Sub msg


port previewRendered : (RenderedPreview -> msg) -> Sub msg
