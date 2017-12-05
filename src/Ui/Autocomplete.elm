module Ui.Autocomplete
    exposing
        ( view
        , update
        , item
        , Model
        , defaultModel
        , defaultConfig
        , subscriptions
        , config
        , effects
        , Msg
        , Msg(..)
        )

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Ui.Textfield as Textfield
import Icons.Icon as Icon
import Html.Events exposing (onClick, onInput, onFocus, onWithOptions)
import Ui.Menu as Menu
import Ui.Options as Options exposing (styled, cs, css, when)
import Mouse
import Dom.Scroll exposing (toTop)
import Ui.Internal.Menu as InternalMenu exposing (Msg(..))
import Task


type alias Model =
    { textfield : Textfield.Model
    , menu : Menu.Model
    }


defaultModel : Model
defaultModel =
    { textfield = Textfield.defaultModel
    , menu = Menu.defaultModel
    }


effects : Cmd Msg
effects =
    Menu.effects |> Cmd.map MenuMsg


type Msg
    = TextfieldMsg Textfield.Msg
    | MenuMsg Menu.Msg
    | Open
    | Click Mouse.Position


update : Msg -> Model -> Maybe String -> ( Model, Maybe String, Cmd Msg )
update msg model selected =
    case msg of
        TextfieldMsg msg_ ->
            let
                ( newTextfieldModel, newText ) =
                    Textfield.externalUpdate
                        msg_
                        model.textfield
                        Textfield.defaultConfig
                        selected
            in
                ( { model | textfield = newTextfieldModel }, newText, Cmd.none )

        MenuMsg msg_ ->
            let
                ( m, menuFx ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = m }, selected, menuFx |> Cmd.map MenuMsg )

        Open ->
            let
                menu =
                    model.menu

                newMenuModel =
                    { menu | open = True }
            in
                ( { model | menu = newMenuModel }
                , selected
                , Cmd.none
                )

        Click pos ->
            case model.textfield.geometry of
                Just geometry ->
                    let
                        inside { x, y } { top, left, width, height } =
                            (left <= toFloat x)
                                && (toFloat x <= left + width)
                                && (top <= toFloat y)
                                && (toFloat y <= top + height)

                        menu =
                            model.menu

                        newMenuModel =
                            { menu | open = False }

                        task =
                            toTop model.menu.id
                                |> Task.attempt ScrollToTopResult
                                |> Cmd.map MenuMsg
                    in
                        if
                            inside pos
                                geometry.textfield.bounds
                        then
                            ( model
                            , selected
                            , Cmd.none
                            )
                        else
                            ( { model | menu = newMenuModel }
                            , selected
                            , task
                            )

                Nothing ->
                    ( model
                    , selected
                    , Cmd.none
                    )


type alias Config =
    { textfieldConfig : Textfield.Config
    , selected : Maybe String
    , width : Int
    , loading : Bool
    }


defaultConfig : Config
defaultConfig =
    { textfieldConfig = Textfield.defaultConfig
    , selected = Nothing
    , width = 380
    , loading = False
    }


config : String -> Maybe String -> Int -> Bool -> Config
config label selected width loading =
    let
        tfConfig =
            Textfield.defaultConfig

        setLabel =
            { tfConfig
                | labelText = Just label
                , width = width
            }
    in
        { textfieldConfig = setLabel
        , selected = selected
        , width = width
        , loading = loading
        }


item : List (Html.Attribute m) -> List (Html m) -> Html m
item attributes children =
    Html.li (class "mdc-list-item" :: attributes) children


view : (Msg -> m) -> Model -> Config -> List (Html m) -> Html m
view lift model { width, textfieldConfig, selected, loading } htmlItems =
    let
        defaultMenuConfig =
            Menu.defaultConfig

        textfieldConfig_ =
            { textfieldConfig
                | helperText =
                    if List.length htmlItems == 0 && selected /= Nothing then
                        "Ничего не найдено"
                    else
                        ""
            }

        menuConfig =
            { defaultMenuConfig
                | width = width
            }

        widthPx =
            (toString width) ++ "px"

        selectedString =
            selected |> Maybe.withDefault ""

        list =
            if String.length selectedString > 2 then
                htmlItems
            else
                []
    in
        div
            [ onClick (lift Open)
            , style
                [ ( "position", "relative" )
                , ( "width", widthPx )
                , ( "height", "48px" )
                , ( "max-height", "48px" )
                , ( "display", "inline-flex" )
                , ( "align-items", "center" )
                ]
            ]
            [ Textfield.view selected
                model.textfield
                textfieldConfig_
                |> Html.map (lift << TextfieldMsg)
            , styled div
                [ css "position" "absolute"
                , css "top" "60px"
                , css "left" "0px"
                , css "height" "250px"
                , css "width" widthPx
                , css "pointer-events"
                    (if model.menu.open then
                        "initial"
                     else
                        "none"
                    )
                ]
                [ Menu.view (lift << MenuMsg) model.menu menuConfig list
                ]
            , if loading then
                styled div
                    [ cs "mdc-linear-progress mdc-linear-progress--reversed"
                    , css "height" "2px"
                    , css "position" "absolute"
                    , css "bottom" "-4px"
                    , css "background-color" "white"
                    ]
                    [ styled div [ cs "mdc-linear-progress__buffering-dots" ] []
                    ]
              else
                Icon.view "arrow_drop_down"
                    [ style
                        [ ( "position", "relative" )
                        , ( "right", "22px" )
                        , ( "top", "10px" )
                        , ( "display"
                          , if model.textfield.isFocused then
                                "none"
                            else
                                "block"
                          )
                        ]
                    ]
            ]



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.menu.open == True then
            Mouse.clicks Click
          else
            Sub.none
        ]
