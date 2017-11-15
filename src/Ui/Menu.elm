module Ui.Menu
    exposing
        ( Model
        , Msg
        , Config
        , Alignment(..)
        , defaultConfig
        , view
        , asIcon
        , defaultModel
        , update
        , onSelect
        , subscriptions
        , attach
        , init
        , effects
        )

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Events as Events
import Html.Attributes as Attrs exposing (style)
import Json.Decode as Json
import Ui.Internal.Menu as InternalMenu
    exposing
        ( Msg(..)
        , Geometry
        , Element
        , element
        , decoder
        )
import Mouse
import Dom
import Task
import Time exposing (Time)
import Dom.Scroll exposing (toTop)
import Utils.Style exposing (mkClass, mkClassList)
import Icons.Icon as Icon


class =
    mkClass "ui-menu--"


classList =
    mkClassList "ui-menu--"


type alias Config =
    { alignment : Alignment
    , width : Int
    }


defaultConfig : Config
defaultConfig =
    { alignment = OpenFromTopLeft
    , width = 184
    }


type Alignment
    = OpenFromTopLeft
    | OpenFromTopRight



-- MODEL


type alias Model =
    { geometry : Maybe Geometry
    , open : Bool
    , top : Float
    , left : Float
    , id : String
    }


defaultModel : Model
defaultModel =
    { geometry = Nothing
    , open = False
    , top = 0
    , left = 0
    , id = ""
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Task.perform CurrentTime Time.now
    )


effects : Cmd Msg
effects =
    Task.perform CurrentTime Time.now


type alias Msg =
    InternalMenu.Msg



-- UPDATE


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd Msg )
update fwd msg model =
    case msg of
        CurrentTime time ->
            let
                id =
                    toString <| Time.inMilliseconds time
            in
                { model | id = id } ! []

        ScrollToTopResult result ->
            ( model, Cmd.none )

        Open ->
            ( { model | open = True }, Cmd.none )

        Close ->
            ( { model | open = False }, Cmd.none )

        Click pos ->
            case model.geometry of
                Just geometry ->
                    let
                        inside { x, y } { top, left, width, height } =
                            (left <= toFloat x)
                                && (toFloat x <= left + width)
                                && (top <= toFloat y)
                                && (toFloat y <= top + height)

                        task =
                            toTop model.id |> Task.attempt ScrollToTopResult
                    in
                        -- if inside pos geometry.menu.bounds then
                        --     ( model, Cmd.none )
                        -- else
                        ({ model | open = False }) ! [ task ]

                Nothing ->
                    ( model, Cmd.none )

        Toggle geom ->
            ( { model
                | open = True
                , geometry = Just geom
              }
            , Cmd.none
            )

        Init geometry ->
            ( { model
                | geometry = Just geometry
              }
            , Cmd.none
            )



-- menu view


asIcon : (Msg -> m) -> Model -> Config -> List (Html m) -> Html m
asIcon lift model config items =
    div
        [ style [ ( "position", "relative" ) ] ]
        [ Icon.asButton "more_vert"
            [ attach lift
            , style
                [ ( "position", "absolute" ) ]
            ]
            [ style [ ( "color", "#000" ) ] ]
        , view lift model config items
        ]


view : (Msg -> m) -> Model -> Config -> List (Html m) -> Html m
view lift { open, left, top, id } config items =
    let
        initOn event =
            Events.on event (Json.map (Init >> lift) decoder)

        transformOriginStyle =
            case config.alignment of
                OpenFromTopLeft ->
                    [ ( "transform-origin", "left top 0px" ) ]

                OpenFromTopRight ->
                    [ ( "transform-origin", "right top 0px" ) ]

        alignmentStyle =
            case config.alignment of
                OpenFromTopLeft ->
                    [ ( "left", "0px" ) ]

                OpenFromTopRight ->
                    [ ( "left", "-148px" ) ]

        menuStyle =
            ( "width", toString config.width ++ "px" )
                :: ( "max-height", "256px" )
                :: alignmentStyle
    in
        div
            [ Attrs.classList
                [ ( "mdc-simple-menu mdc-simple-menu--open", True )
                , ( "elm-mdc-menu--uninitialized", True )
                ]
            , Attrs.id id
            , classList
                [ ( "menu", True )
                , ( "menu--hidden", not open )
                ]
            , style (List.append transformOriginStyle menuStyle)
            , initOn "elm-mdc-init"
            ]
            [ div
                [ classList
                    [ ( "inner1", True ), ( "inner1--hidden", not open ) ]
                , style transformOriginStyle
                ]
                [ div
                    [ classList
                        [ ( "inner2", True ), ( "inner2--hidden", not open ) ]
                    , style transformOriginStyle
                    ]
                    [ ul [ class "mdc-simple-menu__items mdc-list" ]
                        (items)
                    ]
                ]
            ]


attach : (Msg -> msg) -> Attribute msg
attach lift =
    Events.on "click" (Json.map (lift << Toggle) decoder)


onSelect : m -> Attribute m
onSelect msg =
    Events.onClick msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.open == True then
            Mouse.clicks Click
          else
            Sub.none
        ]
