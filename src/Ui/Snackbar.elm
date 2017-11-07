module Ui.Snackbar
    exposing
        ( -- VIEW
          view
        , Property
        , alignStart
        , alignEnd
        , onDismiss
        , Contents
        , toast
        , snack
          -- TEA
        , Model
        , State(..)
        , defaultModel
        , Msg
        , update
          -- , add
        )

import Dict
import Html.Attributes as Html
import Html exposing (Html, text)
import Ui.Internal.Helpers as Helpers exposing (map1st, delay, cmd)
import Ui.Internal.Options as Internal
import Ui.Internal.Snackbar exposing (Msg(..), Transition(..))
import Ui.Options as Options exposing (styled, cs, when)
import Maybe exposing (andThen)
import Platform.Cmd exposing (Cmd)
import Time exposing (Time)


-- MODEL


type alias Contents =
    { message : String
    , action : Maybe String
    , timeout : Time
    , fade : Time
    , multiline : Bool
    , actionOnBottom : Bool
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


type alias Msg m =
    Ui.Internal.Snackbar.Msg m


{-| Generate toast with given message. Timeout is 2750ms, fade 250ms.
-}
toast : String -> Contents
toast message =
    { message = message
    , action = Nothing
    , timeout = 2750
    , fade = 250
    , multiline = False
    , actionOnBottom = False
    , dismissOnAction = True
    }


{-| Generate snack with given message and label.
Timeout is 2750ms, fade 250ms.
-}
snack : String -> String -> Contents
snack message label =
    { message = message
    , action = Just label
    , timeout = 2750
    , fade = 250
    , multiline = True
    , actionOnBottom = False
    , dismissOnAction = True
    }



-- SNACKBAR STATE MACHINE


type alias Transition =
    Ui.Internal.Snackbar.Transition


type State
    = Inert
    | Active Contents
    | Fading Contents


next : Model -> Cmd Transition -> Cmd (Msg m)
next model =
    Cmd.map (Move model.seq)


move : Transition -> Model -> ( Model, Cmd (Msg m) )
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


tryDequeue : Model -> ( Model, Cmd (Msg m) )
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


{-| Elm Architecture update function.
-}
update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update fwd msg model =
    case msg of
        Move seq transition ->
            if seq == model.seq then
                move transition model |> Helpers.map2nd (Cmd.map fwd)
            else
                model ! []

        Dismiss dismissOnAction actionOnDismiss ->
            let
                fwdEffect =
                    case actionOnDismiss of
                        Just msg_ ->
                            cmd msg_

                        Nothing ->
                            Cmd.none
            in
                (if dismissOnAction then
                    update fwd (Move model.seq Clicked) model
                 else
                    model ! []
                )
                    |> Helpers.map2nd (\cmd -> Cmd.batch [ cmd, fwdEffect ])


{-| Add a message to the snackbar. If another message is currently displayed,
the provided message will be queued. You will be able to observe a `Begin` action
(see `Msg` above) once the action begins displaying.

You must dispatch the returned effect for the Snackbar to begin displaying your
message.

-}



-- add : (Material.Msg.Msg m -> m) -> Contents -> { a | mdl : Store s } -> ( { a | mdl : Store s }, Cmd m )
-- add lift idx contents model =
--     let
--         component_ =
--             Dict.get idx model.mdl.snackbar
--                 |> Maybe.withDefault defaultModel
--
--         ( component, effects ) =
--             enqueue contents component_ |> tryDequeue
--
--         mdl =
--             let
--                 mdl_ =
--                     model.mdl
--             in
--                 { mdl_ | snackbar = Dict.insert idx component mdl_.snackbar }
--     in
--         { model | mdl = mdl } ! [ Cmd.map (Material.Msg.SnackbarMsg idx >> lift) effects ]
--
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


alignStart : Property m
alignStart =
    Options.cs "mdc-snackbar--align-start"


alignEnd : Property m
alignEnd =
    Options.cs "mdc-snackbar--align-end"


view : (Msg m -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
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

        multiline =
            (Maybe.map .multiline contents == Just True)

        actionOnBottom =
            (Maybe.map .actionOnBottom contents == Just True)
                && multiline

        dismissHandler =
            case ( contents, config.onDismiss ) of
                ( Just content, Just _ ) ->
                    Options.onClick (lift (Dismiss content.dismissOnAction config.onDismiss))

                _ ->
                    Options.nop

        ({ config } as summary) =
            Internal.collect defaultConfig options
    in
        Internal.apply summary
            Html.div
            [ cs "mdc-snackbar"
            , cs "mdc-snackbar--active"
                |> when isActive
            , cs "mdc-snackbar--multiline"
                |> when multiline
            , cs "mdc-snackbar--action-on-bottom"
                |> when actionOnBottom
            ]
            []
            [ styled Html.div
                [ cs "mdc-snackbar__text"
                ]
                (contents
                    |> Maybe.map (\c -> [ text c.message ])
                    |> Maybe.withDefault []
                )
            , styled Html.div
                [ cs "mdc-snackbar__action-wrapper"
                ]
                [ Options.styled_ Html.button
                    [ cs "mdc-button"
                    , cs "mdc-snackbar__action-button"
                    , dismissHandler
                    ]
                    [ Html.type_ "button"
                    ]
                    (action
                        |> Maybe.map (\action -> [ text action ])
                        |> Maybe.withDefault []
                    )
                ]
            ]


type alias Property m =
    Options.Property (Config m) m


type alias Store s =
    { s
        | snackbar : Model
    }
