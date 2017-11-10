module Ui.Select
    exposing
        ( view
        , viewEditable
        , update
        , item
        , Model
        , defaultModel
        , defaultConfig
        , subscriptions
        , config
        , Msg
        , Msg(..)
        , effects
        )

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Ui.Textfield as Textfield
import Icons.Icon as Icon
import Ui.Menu as Menu


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
                ( m, fx ) =
                    Menu.update MenuMsg msg_ model.menu
            in
                ( { model | menu = m }, selected, fx |> Cmd.map MenuMsg )


type alias Config =
    { textfieldConfig : Textfield.Config
    , selected : Maybe String
    , width : Int
    }


defaultConfig : Config
defaultConfig =
    { textfieldConfig = Textfield.defaultConfig
    , selected = Nothing
    , width = 380
    }


config : String -> Maybe String -> Int -> Config
config label selected width =
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
        }


item : List (Html.Attribute m) -> List (Html m) -> Html m
item attributes children =
    Html.li (class "mdc-list-item" :: attributes) children


viewEditable : (Msg -> m) -> Model -> Config -> List (Html m) -> Html m
viewEditable lift model { width, textfieldConfig, selected } htmlItems =
    let
        defaultMenuConfig =
            Menu.defaultConfig

        menuConfig =
            { defaultMenuConfig
                | width = width
            }
    in
        div
            [ -- Menu.attach (lift << MenuMsg)
              style
                [ ( "position", "relative" )
                , ( "width", "298px" )
                , ( "height", "48px" )
                , ( "max-height", "48px" )
                , ( "display", "inline-flex" )
                , ( "align-items", "center" )
                ]
            ]
            [ Textfield.view selected
                model.textfield
                textfieldConfig
                |> Html.map (lift << TextfieldMsg)
            , Menu.view (lift << MenuMsg) model.menu menuConfig htmlItems
            , Icon.asButton "arrow_drop_down"
                [ style
                    [ ( "position", "relative" )
                    , ( "right", "22px" )
                    , ( "top", "7px" )
                    ]
                , Menu.attach (lift << MenuMsg)
                ]
                []
            ]


view : (Msg -> m) -> Model -> Config -> List (Html m) -> Html m
view lift model { width, textfieldConfig, selected } htmlItems =
    let
        defaultMenuConfig =
            Menu.defaultConfig

        menuConfig =
            { defaultMenuConfig
                | width = width
            }
    in
        div
            [ style
                [ ( "height", "54px" )
                , ( "min-height", "54px" )
                , ( "border-bottom", "1px solid rgba(0, 0, 0, 0.12)" )
                , ( "width", (toString width) ++ "px" )

                -- , ( "padding-bottom"
                --   , if selected == Nothing then
                --         "4px"
                --     else
                --         "0px"
                --   )
                ]
            ]
            [ div
                [ Menu.attach (lift << MenuMsg)
                , class "ui-select"
                , style
                    [ ( "position", "relative" )
                    , ( "width", (toString width) ++ "px" )
                    , ( "height", "52px" )
                    , ( "display", "inline-flex" )
                    , ( "align-items", "center" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ Textfield.viewReadonly
                    selected
                    model.textfield
                    textfieldConfig
                    |> Html.map never
                , Menu.view (lift << MenuMsg) model.menu menuConfig htmlItems
                , Icon.asButton "arrow_drop_down"
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "24px" )
                        , ( "right", "0px" )
                        ]
                    ]
                    []
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MenuMsg (Menu.subscriptions model.menu)
        ]
