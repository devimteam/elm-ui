module Ui.Snackbar
    exposing
        ( view
        , Property
        , onDismiss
        , Contents
        , toast
        , Model
        , State(..)
        , Msg
        , defaultModel
        , update
        , add
        )

import Html exposing (Html, text)
import Ui.Internal.Helpers as Helpers exposing (map1st, delay, cmd)
import Ui.Internal.Options as Internal
import Ui.Internal.Snackbar exposing (Msg(..), Transition(..))
import Ui.Options as Options exposing (styled, cs, when)
import Maybe exposing (andThen)
import Platform.Cmd exposing (Cmd)
import Time exposing (Time)


type alias Msg =
    Ui.Internal.Snackbar.Msg



-- MODEL


type alias Contents =
    { message : String
    , action : Maybe String
    , timeout : Time
    , fade : Time
    , dismissOnAction : Bool
    }


type alias Model =
    { queue : List Contents
    , state : State
    , seq : Int
    }


defaultModel : Model
defaultModel =
    { queue = []
    , state = Inert
    , seq = -1
    }


{-| Generate toast with given message. Timeout is 2750ms, fade 250ms.
-}
toast : String -> Contents
toast message =
    { message = message
    , action = Nothing
    , timeout = 2750
    , fade = 250
    , dismissOnAction = False
    }



-- SNACKBAR STATE MACHINE


type alias Transition =
    Ui.Internal.Snackbar.Transition


type State
    = Inert
    | Active Contents
    | Fading Contents


next : Model -> Cmd Transition -> Cmd Msg
next model =
    Cmd.map (Move model.seq)


move : Transition -> Model -> ( Model, Cmd Msg )
move transition model =
    case ( model.state, transition ) of
        ( Inert, Timeout ) ->
            tryDequeue model

        ( Active contents, Clicked ) ->
            { model
                | state = Fading contents
            }
                ! [ delay contents.fade Timeout |> next model ]

        ( Active contents, Timeout ) ->
            { model
                | state = Fading contents
            }
                ! [ delay contents.fade Timeout |> next model ]

        ( Fading contents, Timeout ) ->
            { model
                | state = Inert
            }
                ! [ cmd Timeout |> next model ]

        _ ->
            model ! []



-- NOTIFICATION QUEUE


enqueue : Contents -> Model -> Model
enqueue contents model =
    { model
        | queue = List.append model.queue [ contents ]
    }


tryDequeue : Model -> ( Model, Cmd Msg )
tryDequeue model =
    case ( model.state, model.queue ) of
        ( Inert, c :: cs ) ->
            ( { model
                | state = Active c
                , queue = cs
                , seq = model.seq + 1
              }
            , Cmd.batch
                [ delay c.timeout Timeout |> Cmd.map (Move (model.seq + 1))
                ]
            )

        _ ->
            model ! []



-- ACTIONS, UPDATE


update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update fwd msg model =
    case msg of
        Move seq transition ->
            if seq == model.seq then
                move transition model |> Helpers.map2nd (Cmd.map fwd)
            else
                model ! []


add : (Msg -> m) -> Contents -> Model -> ( Model, Cmd m )
add lift contents model =
    let
        ( newModel, effects ) =
            enqueue contents model |> tryDequeue
    in
        newModel ! [ Cmd.map lift effects ]



-- VIEW


type alias Config m =
    { onDismiss : Maybe m
    }


defaultConfig : Config m
defaultConfig =
    { onDismiss = Nothing
    }


onDismiss : m -> Property m
onDismiss =
    Internal.option << (\msg config -> { config | onDismiss = Just msg })


view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options _ =
    let
        contents =
            case model.state of
                Inert ->
                    Nothing

                Active c ->
                    Just c

                Fading c ->
                    Just c

        isActive =
            case model.state of
                Inert ->
                    False

                Active _ ->
                    True

                Fading _ ->
                    False

        action =
            contents |> Maybe.andThen .action

        trim =
            String.slice 0 150
    in
        styled Html.div
            [ cs "mdc-snackbar"
            , cs "mdc-snackbar--active"
                |> when isActive
            ]
            [ styled Html.div
                [ cs "mdc-snackbar__text"
                ]
                (contents
                    |> Maybe.map (\c -> [ text <| trim c.message ])
                    |> Maybe.withDefault []
                )
            ]


type alias Property m =
    Options.Property (Config m) m
