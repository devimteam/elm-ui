module Ui.Slider
    exposing
        ( view
        , Model
        , defaultModel
        , defaultConfig
        , Msg
        , update
        , subscriptions
        , Config
        , targetValue
        , onInput
        , discretize
        )

import Html exposing (Html, text, div, button, Attribute, ul, li)
import Html.Attributes as Attr exposing (tabindex, class, classList, style)
import Html.Events as Events
import Json.Decode as Json exposing (Decoder)
import Svg
import Svg.Attributes as Svg
import Ui.Internal.Slider as InternalSlider exposing (Msg(..), Geometry, defaultGeometry)
import DOM
import Mouse


onInput : Decoder m -> Attribute m
onInput =
    Events.on "input"


defaultConfig : Config
defaultConfig =
    { value = 0
    , min = 0
    , max = 20
    , steps = 5
    , discrete = False
    , trackMarkers = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        list =
            (if model.active == True then
                [ Mouse.moves MouseDrag, Mouse.ups MouseUp ]
             else
                []
            )
    in
        Sub.batch list


type alias Model =
    { focus : Bool
    , active : Bool
    , touched : Bool
    , geometry : Maybe Geometry
    , value : Maybe Float
    , inTransit : Bool
    , initialized : Bool
    , requestAnimation : Bool
    }


defaultModel : Model
defaultModel =
    { focus = False
    , active = False
    , touched = False
    , geometry = Nothing
    , value = Nothing
    , inTransit = False
    , initialized = False
    , requestAnimation = True
    }


type alias Msg =
    InternalSlider.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Input val ->
            ( model, Cmd.none )

        SetValue val ->
            ( { model | value = Just val }, Cmd.none )

        Focus ->
            ( { model | focus = True }, Cmd.none )

        Blur ->
            ( { model | focus = False, active = False }, Cmd.none )

        Tick ->
            ( { model | inTransit = False }, Cmd.none )

        Activate inTransit geometry ->
            ( { model
                | active = True
                , geometry = Just geometry
                , inTransit = inTransit
                , value = Just (computeValue geometry)
              }
            , Cmd.none
            )

        Init geometry ->
            ( { model
                | geometry = Just geometry
                , value = Just (computeValue geometry)
                , initialized = True
              }
            , Cmd.none
            )

        Up ->
            ( model
            , Cmd.none
            )

        MouseUp pos ->
            ( { model
                | active = False
              }
            , Cmd.none
            )

        MouseDrag pos ->
            let
                g =
                    model.geometry |> Maybe.withDefault defaultGeometry

                newGeometry =
                    { g | x = (toFloat pos.x) }

                value =
                    Just (computeValue newGeometry)
            in
                ( { model
                    | geometry = Just newGeometry
                    , value = value
                  }
                , Cmd.none
                )

        Resize ->
            ( { model | requestAnimation = True }
            , Cmd.none
            )

        AnimationFrame ->
            if model.requestAnimation then
                ( { model
                    | requestAnimation = False
                    , initialized = False
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


type alias Config =
    { value : Float
    , min : Float
    , max : Float
    , discrete : Bool
    , steps : Int
    , trackMarkers : Bool
    }


discretize : Int -> Float -> Float
discretize steps continuousValue =
    toFloat (steps * round (continuousValue / toFloat steps))


computeValue : Geometry -> Float
computeValue geometry =
    let
        c =
            if geometry.width /= 0 then
                (geometry.x - geometry.left) / geometry.width
            else
                0
                    |> clamp 0 1
    in
        geometry.min
            + c
            * (geometry.max - geometry.min)
            |> clamp geometry.min geometry.max


decodeGeometry : Decoder Geometry
decodeGeometry =
    Json.oneOf
        [ Json.at [ "pageX" ] Json.float
        , Json.succeed 0
        ]
        |> Json.andThen
            (\x ->
                DOM.target <|
                    traverseToContainer <|
                        Json.map6
                            (\offsetWidth offsetLeft discrete min max steps ->
                                { width = offsetWidth
                                , left = offsetLeft
                                , x = x
                                , discrete = discrete
                                , min = min
                                , max = max
                                , steps = steps
                                }
                            )
                            DOM.offsetWidth
                            DOM.offsetLeft
                            (hasClass "mdc-slider--discrete")
                            (data "min" (Json.map (String.toFloat >> Result.withDefault 1) Json.string))
                            (data "max" (Json.map (String.toFloat >> Result.withDefault 1) Json.string))
                            (data "steps" (Json.map (String.toInt >> Result.withDefault 1) Json.string))
            )



-- (DOM.target (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight))))


data : String -> Decoder a -> Decoder a
data key decoder =
    Json.at [ "dataset", key ] decoder


traverseToContainer : Decoder a -> Decoder a
traverseToContainer decoder =
    hasClass "mdc-slider"
        |> Json.andThen
            (\doesHaveClass ->
                if doesHaveClass then
                    decoder
                else
                    DOM.parentElement (Json.lazy (\_ -> traverseToContainer decoder))
            )


hasClass : String -> Decoder Bool
hasClass class =
    Json.map
        (\className ->
            String.contains (" " ++ class ++ " ") (" " ++ className ++ " ")
        )
        (Json.at [ "className" ] Json.string)


onMouseDown : (Geometry -> msg) -> Attribute msg
onMouseDown msg =
    Events.onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = False
        }
        (Json.map msg decodeGeometry)


dataAttr : String -> String -> Attribute msg
dataAttr key val =
    Attr.attribute ("data-" ++ key) val


targetValue : Decoder Float
targetValue =
    Json.map
        (\geometry ->
            discretize geometry.steps (computeValue geometry)
        )
        decodeGeometry


view : Model -> Config -> Html Msg
view model config =
    let
        continuousValue =
            if model.active then
                model.value
                    |> Maybe.withDefault config.value
            else
                config.value

        value =
            discretize config.steps continuousValue

        translateX =
            let
                v =
                    value
                        |> clamp config.min config.max

                c =
                    if (config.max - config.min) /= 0 then
                        (v - config.min)
                            / (config.max - config.min)
                            |> clamp 0 1
                    else
                        0
            in
                model.geometry
                    |> Maybe.map .width
                    |> Maybe.map ((*) c)
                    |> Maybe.withDefault 0

        activateOn event =
            Events.on event (Json.map (Activate True) decodeGeometry)

        initOn event =
            Events.on event (Json.map (Init) decodeGeometry)

        ups =
            [ "mouseup"
            , "touchend"
            , "pointerup"
            ]

        downs =
            [ "mousedown"
            , "touchstart"
            , "keydown"
            , "pointerdown"
            ]

        moves =
            [ "mousemove"
            , "touchmove"
            , "pointermove"
            ]

        leaves =
            [ "mouseleave"
            , "touchleave" -- TODO
            , "pointerleave" -- TODO
            ]

        activateOn_ event =
            Events.onWithOptions event
                { stopPropagation = True
                , preventDefault = False
                }
                (Json.map (Activate False) decodeGeometry)

        trackScale =
            if config.max - config.min == 0 then
                0
            else
                (value - config.min) / (config.max - config.min)
    in
        div
            ([ classList
                [ ( "mdc-slider mdc-slider--discrete elm-mdc-slider--uninitialized", True )
                , ( "mdc-slider--focus", model.focus )
                , ( "mdc-slider--active", model.active )
                , ( "mdc-slider--in-transit", model.inTransit )
                , ( "mdc-slider--off", value <= config.min )
                ]
             , tabindex 0
             , Events.on "focus" (Json.succeed Focus)
             , Events.on "blur" (Json.succeed Blur)
             , dataAttr "min" (toString config.min)
             , dataAttr "max" (toString config.max)
             , dataAttr "steps" (toString config.steps)
             , initOn "elm-mdc-init"
             ]
                ++ List.map activateOn downs
                ++ [ style [ ( "bottom", "8px" ), ( "height", "inherit" ) ] ]
            )
            [ div [ class "mdc-slider__track-container" ]
                [ div
                    [ class "mdc-slider__track"
                    , style [ ( "transform", "scaleX(" ++ toString trackScale ++ ")" ) ]
                    ]
                    []
                , div [ class "mdc-slider__track-marker-container" ]
                    (List.repeat ((round (config.max - config.min)) // config.steps) <|
                        div [ class "mdc-slider__track-marker" ] []
                    )
                ]
            , div
                [ class "mdc-slider__thumb-container"
                , onMouseDown (Activate False)
                , style
                    [ ( "transform", "translateX(" ++ toString translateX ++ "px) translateX(-50%)" )
                    , ( "top", "7px" )
                    ]
                ]
                [ Svg.svg
                    [ Svg.class "mdc-slider__thumb"
                    , Svg.width "21"
                    , Svg.height "21"
                    ]
                    [ Svg.circle
                        [ Svg.cx "10.5"
                        , Svg.cy "10.5"
                        , Svg.r "7.875"
                        ]
                        []
                    ]
                , div
                    [ class "mdc-slider__focus-ring" ]
                    []
                ]
            ]
