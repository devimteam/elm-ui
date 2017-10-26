module Ui.Internal.Menu
    exposing
        ( Geometry
        , Element
        , decoder
        , element
        , Msg(..)
        )

import DOM
import Json.Decode exposing (..)
import Mouse


type Msg
    = Open
    | Close
    | Toggle Geometry
    | Click Mouse.Position
    | Init Geometry


type alias Geometry =
    { menu : Element
    }


type alias Element =
    { offsetTop : Float
    , offsetLeft : Float
    , offsetHeight : Float
    , bounds : DOM.Rectangle
    }


element : Decoder Element
element =
    map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


decoder : Decoder Geometry
decoder =
    map Geometry (DOM.target element)
