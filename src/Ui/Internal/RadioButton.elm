module Ui.Internal.RadioButton exposing (Msg(..))

import Ui.Internal.Ripple as Ripple


type Msg
    = RippleMsg Ripple.Msg
    | SetFocus Bool
    | NoOp
