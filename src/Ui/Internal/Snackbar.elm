module Ui.Internal.Snackbar exposing (Msg(..), Transition(..))


type Msg
    = Move Int Transition


type Transition
    = Timeout
    | Clicked
