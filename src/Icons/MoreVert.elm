module Icons.MoreVert exposing (view)

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
        [ path [ d "M0 0h24v24H0z", fill "none" ] []
        , path [ d "M12 8c1.1 0 2-.9 2-2s-.9-2-2-2-2 .9-2 2 .9 2 2 2zm0 2c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm0 6c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2z" ] []
        ]
