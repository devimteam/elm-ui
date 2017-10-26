module Icons.ArrowUpward exposing (view)

import Html exposing (Html)
import Svg exposing (svg, path)
import Svg.Attributes exposing (class, fill, stroke, d, viewBox, width, height)


view : String -> Html msg
view cn =
    svg
        [ class cn
        , fill "#000000"
        , height "24"
        , width "24"
        , viewBox "0 0 24 24"
        ]
        [ path [ d "M0 0h24v24H0V0z", fill "none" ] []
        , path [ d "M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" ] []
        ]
