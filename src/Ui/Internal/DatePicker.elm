module Ui.Internal.DatePicker
    exposing
        ( Geometry
        , Element
        , geometryDecoder
        , element
        )

import DOM
import Json.Decode exposing (..)
import Html.Events exposing (targetValue)
import Mouse


type alias Geometry =
    { picker : Element
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
    map Geometry
        (DOM.target <| DOM.nextSibling (DOM.parentElement element))


{-| Decode an Element
-}
element : Decoder Element
element =
    map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect
