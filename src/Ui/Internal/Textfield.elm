module Ui.Internal.Textfield
    exposing
        ( Msg(..)
        , Geometry
        , Element
        , geometryDecoder
        , element
        )

import MaskedInput.Text as MaskedText
import DOM
import Json.Decode as Json exposing (Decoder)
import Html.Events exposing (targetValue)


type alias Geometry =
    { textfield : Element
    , picker : Element
    }


type alias Element =
    { offsetTop : Float
    , offsetLeft : Float
    , offsetHeight : Float
    , bounds : DOM.Rectangle
    }


{-| Decode Geometry
-}
geometryDecoder : Decoder Geometry
geometryDecoder =
    Json.map2 Geometry
        (DOM.target element)
        (DOM.target <| DOM.parentElement <| DOM.parentElement <| DOM.nextSibling element)


{-| Decode an Element
-}
element : Decoder Element
element =
    Json.map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


type Msg
    = Blur
    | Focus
    | InputClick Geometry
    | Input String
    | CurrencyInput (Maybe String)
    | SubmitText
    | SetValue String
    | NoOp
    | FocusChanged Bool
    | InputStateChanged MaskedText.State
