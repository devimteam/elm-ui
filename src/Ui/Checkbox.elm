module Ui.Checkbox
    exposing
        ( -- VIEW
          view
        , Property
        , disabled
        , checked
        , indeterminate
          -- TEA
        , Model
        , defaultModel
        , Msg
        , update
        , invalid
        )

import Html.Attributes as Html
import Html exposing (Html, text, div)
import Json.Decode as Json
import Json.Encode
import Ui.Internal.Helpers exposing (map1st, map2nd, blurOn, filter, noAttr)
import Ui.Internal.Checkbox exposing (Msg(..))
import Ui.Internal.Options as Internal
import Svg.Attributes as SvgAttr
import Svg exposing (path)
import Ui.Options as Options exposing (Style, cs, css, styled, many, when, maybe)
import Ui.Ripple as Ripple


type alias Model =
    { ripple : Ripple.Model
    , isFocused : Bool
    }


defaultModel : Model
defaultModel =
    { ripple = Ripple.defaultModel
    , isFocused = False
    }


type alias Msg =
    Ui.Internal.Checkbox.Msg


update : (Msg -> m) -> Msg -> Model -> ( Maybe Model, Cmd m )
update lift msg model =
    case msg of
        RippleMsg msg_ ->
            let
                ( ripple, effects ) =
                    Ripple.update msg_ model.ripple
            in
                ( Just { model | ripple = ripple }
                , Cmd.map (RippleMsg >> lift) effects
                )

        SetFocus focus ->
            ( Just { model | isFocused = focus }, Cmd.none )

        NoOp ->
            ( Nothing, Cmd.none )


type alias Config m =
    { input : List (Options.Style m)
    , container : List (Options.Style m)
    , value : Bool
    }


defaultConfig : Config m
defaultConfig =
    { input = []
    , container = []
    , value = False
    }


type alias Property m =
    Options.Property (Config m) m


disabled : Property m
disabled =
    Options.many
        [ cs "mdc-checkbox--disabled"
        , Internal.input
            [ Internal.attribute <| Html.disabled True
            ]
        ]


invalid : Property m
invalid =
    Options.many
        [ cs "mdc-checkbox--invalid"
        ]


checked : Property m
checked =
    Internal.option (\config -> { config | value = True })


indeterminate : Property m
indeterminate =
    Internal.input
        [ Internal.attribute <| Html.property "indeterminate" (Json.Encode.bool True)
        ]


view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options ch =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig options

        ( rippleOptions, rippleStyle ) =
            Ripple.view True (RippleMsg >> lift) model.ripple [] []

        checkmarkClass =
            "mdc-checkbox__checkmark "
                ++ (if config.value then
                        "mdc-checkbox__checkmark--checked"
                    else
                        ""
                   )
    in
        styled Html.div
            [ cs "mdc-form-field" ]
            ([ Internal.applyContainer summary
                Html.div
                [ cs "mdc-checkbox"
                , Internal.attribute <| blurOn "mouseup"
                , rippleOptions
                ]
                [ Internal.applyInput summary
                    Html.input
                    [ cs "mdc-checkbox__native-control"
                    , Internal.attribute <| Html.type_ "checkbox"
                    , Internal.attribute <| Html.checked config.value
                    , Internal.on1 "focus" lift (SetFocus True)
                    , Internal.on1 "blur" lift (SetFocus False)
                    , Options.onWithOptions "click"
                        { preventDefault = True
                        , stopPropagation = False
                        }
                        (Json.succeed (lift NoOp))
                    ]
                    []
                , styled Html.div
                    [ cs "mdc-checkbox__background"
                    , cs "mdc-checkbox__background--checked"
                        |> Options.when
                            config.value
                    ]
                    [ Svg.svg
                        [ SvgAttr.class checkmarkClass
                        , SvgAttr.viewBox "0 0 24 24"
                        ]
                        [ path
                            [ SvgAttr.class "mdc-checkbox__checkmark__path"
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke "white"
                            , SvgAttr.d "M1.73,12.91 8.1,19.28 22.79,4.59"
                            , SvgAttr.style
                                (if config.value then
                                    "stroke-dashoffset:0"
                                 else
                                    ""
                                )
                            ]
                            []
                        ]
                    , styled Html.div
                        [ cs "mdc-checkbox__mixedmark"
                        ]
                        []
                    ]
                , rippleStyle
                ]
             ]
                ++ ch
            )
