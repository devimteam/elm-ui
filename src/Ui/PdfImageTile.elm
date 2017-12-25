port module Ui.PdfImageTile exposing (..)

import Html exposing (Html, div, input, label, text, img, span, p, node)
import Html.Attributes exposing (type_, id, src, for, style, accept, disabled, attribute)
import Html.Events exposing (on, onClick, onWithOptions)
import Json.Decode as JD
import Ui.Options as Options exposing (styled, cs, css, when)
import Icons.Icon as Icon
import Ui.Typography as Typography
import Html.Attributes as Attrs


type alias InputId =
    String


type alias Config =
    { readonly : Bool
    , invalid : Bool
    , inputId : String
    , title : String
    }


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
    | RenderError String


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


renderImage : FileRecord -> Bool -> Html Msg
renderImage file readonly =
    let
        filename_ =
            file.meta.fileName

        url_ =
            Maybe.withDefault "" file.preview

        thumbStyle =
            [ ( "width", "168px" )
            , ( "cursor", "pointer" )
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

        loader =
            div
                [ Attrs.style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "bottom", "0" )
                    , ( "left", "0" )
                    , ( "right", "0" )
                    , ( "z-index", "999" )
                    ]
                ]
                [ div
                    [ Attrs.style
                        [ ( "display", "flex" )
                        , ( "align-items", "center" )
                        , ( "justify-content", "center" )
                        , ( "background-color", "black" )
                        , ( "opacity", "0.33" )
                        , ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "bottom", "0" )
                        , ( "left", "0" )
                        , ( "right", "0" )
                        , ( "z-index", "999" )
                        ]
                    ]
                    [ div
                        [ Attrs.style
                            [ ( "height", "28px" )
                            , ( "width", "28px" )
                            , ( "animation", "rotate 0.8s infinite linear" )
                            , ( "border", "3px solid #fff" )
                            , ( "border-right-color", "transparent" )
                            , ( "border-radius", "50%" )
                            ]
                        ]
                        []
                    ]
                ]

        deleteBtn =
            case readonly of
                False ->
                    Icon.asButton "delete"
                        [ onWithOptions "click"
                            { stopPropagation = True
                            , preventDefault = False
                            }
                            (JD.succeed Clear)
                        ]
                        [ style deleteIconStyle ]

                True ->
                    text ""

        infoDiv =
            case readonly of
                False ->
                    div [ style thumbInfoStyle ]
                        [ span [ style titleStyle ] [ text filename_ ]
                        , div [ style btnStyles ]
                            [ deleteBtn
                            ]
                        ]

                True ->
                    text ""

        thumb =
            div
                [ style thumbStyle
                , onClick (OpenImage file)
                ]
                [ infoDiv
                ]
    in
        div
            [ Attrs.style
                [ ( "position", "relative" )
                , ( "width", "168px" )
                , ( "height", "120px" )
                ]
            ]
            [ if file.preview == Nothing then
                loader
              else
                thumb
            ]


renderPlaceholder : Config -> Html Msg
renderPlaceholder { inputId, readonly, invalid } =
    let
        placeholderStyle =
            [ ( "background-color", "#EDEFF1" )
            , ( "width", "168px" )
            , ( "height", "120px" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "center" )
            ]

        wrapperStyle =
            if invalid then
                [ ( "border", "1px solid red" )
                , ( "width", "168px" )
                ]
            else
                []

        event =
            case readonly of
                False ->
                    [ on "change" (JD.succeed (ImageSelected inputId)) ]

                True ->
                    [ disabled readonly ]

        inputAttrs =
            event
                ++ [ type_ "file"
                   , id inputId
                   , accept ".pdf"
                   , style
                        [ ( "display", "none" )
                        ]
                   ]

        icon =
            case readonly of
                False ->
                    Icon.view "add" []

                True ->
                    Icon.view "photo" []

        errorText =
            styled p
                [ cs "mdc-textfield-helptext mdc-textfield-helptext--validation-msg mdc-textfield-helptext--persistent"
                , css
                    "padding-top"
                    "12px"
                ]
                [ text "Обязательное поле" ]
    in
        div []
            [ div
                [ style wrapperStyle ]
                [ label
                    [ style placeholderStyle
                    , for inputId
                    ]
                    [ icon ]
                , input
                    inputAttrs
                    []
                ]
            , if invalid then
                errorText
              else
                text ""
            ]


view : Config -> Model -> Html Msg
view ({ readonly, title } as config) model =
    let
        body =
            case model.file of
                Just file ->
                    renderImage file readonly

                Nothing ->
                    renderPlaceholder config
    in
        div []
            [ div []
                [ styled div
                    [ Typography.headline, Typography.pad24 ]
                    [ text title ]
                ]
            , body
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RenderError _ ->
            model ! []

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


port errorDuringRender : (String -> msg) -> Sub msg
