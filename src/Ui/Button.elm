module Ui.Button
    exposing
        ( view
        , Property
        , disabled
        , ripple
        , raised
        , unelevated
        , stroked
        , compact
        , dense
        , primary
        , secondary
        , danger
        , link
        , darkTheme
          -- TEA
        , Model
        , defaultModel
        , Msg
        , update
        , noMargin
        )

import Html.Attributes exposing (..)
import Html exposing (Html, text, div)
import Json.Decode as Json
import Ui.Internal.Helpers as Helpers exposing (map1st, map2nd, blurOn, filter, noAttr)
import Ui.Internal.Options as Internal
import Ui.Internal.Button exposing (Msg(..))
import Ui.Options as Options exposing (Style, cs, styled, many, when, maybe, css)
import Ui.Ripple as Ripple


-- MODEL


type alias Model =
    { ripple : Ripple.Model
    }


defaultModel : Model
defaultModel =
    { ripple = Ripple.defaultModel
    }



-- ACTION, UPDATE


type alias Msg =
    Ui.Internal.Button.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RippleMsg msg_ ->
            let
                ( ripple, effects ) =
                    Ripple.update msg_ model.ripple
            in
                ( { model | ripple = ripple }, Cmd.map RippleMsg effects )



-- OPTIONS


type alias Config =
    { ripple : Bool
    , link : Maybe String
    , disabled : Bool
    }


defaultConfig : Config
defaultConfig =
    { ripple = False
    , link = Nothing
    , disabled = False
    }


type alias Property m =
    Options.Property Config m


disabled : Property m
disabled =
    Internal.option (\options -> { options | disabled = True })


raised : Property m
raised =
    cs "mdc-button--raised"


ripple : Property m
ripple =
    Internal.option (\options -> { options | ripple = True })


compact : Property m
compact =
    cs "mdc-button--compact"


dense : Property m
dense =
    cs "mdc-button--dense"


primary : Property m
primary =
    cs "mdc-button--primary"


secondary : Property m
secondary =
    cs "mdc-button--accent"


unelevated : Property m
unelevated =
    cs "mdc-button--unelevated"


stroked : Property m
stroked =
    cs "mdc-button--stroked"


danger : Property m
danger =
    css "color" "#d50000"


noMargin : Property m
noMargin =
    Options.many
        [ css "margin" "0"
        , css "padding" "0"
        ]


darkTheme : Property m
darkTheme =
    cs "mdc-button--dark-theme"


link : String -> Property m
link href =
    Internal.option (\options -> { options | link = Just href })



-- VIEW


view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options nodes =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig options

        ( rippleOptions, rippleStyles ) =
            Ripple.view False (RippleMsg >> lift) model.ripple () ()
    in
        Internal.apply summary
            (if config.link /= Nothing then
                Html.a
             else
                Html.button
            )
            [ cs "mdc-button"
            , cs "mdc-js-button"
            , cs "mdc-js-ripple-effect" |> when summary.config.ripple
            , Options.css "box-sizing" "border-box"
            , Internal.attribute (Html.Attributes.href (Maybe.withDefault "" config.link))
                |> when ((config.link /= Nothing) && not config.disabled)
            , Internal.attribute (Html.Attributes.disabled True)
                |> when config.disabled
            , cs "mdc-button--disabled"
                |> when config.disabled
            , rippleOptions
                |> when config.ripple
            ]
            [ Helpers.blurOn "mouseup"
            , Helpers.blurOn "mouseleave"
            , Helpers.blurOn "touchend"
            ]
            (nodes ++ [ rippleStyles ])
