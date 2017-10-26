module Icons.Icon exposing (view, asButton)

import Html exposing (i, text, Html, Attribute, button)
import Html.Attributes exposing (class, style)


asButton : String -> List (Attribute msg) -> List (Attribute msg) -> Html msg
asButton name buttonAttrs iconAttrs =
    button
        ([ style
            [ ( "width", "24px" )
            , ( "height", "24px" )
            , ( "background-color", "rgba(0, 0, 0, 0)" )
            , ( "cursor", "pointer " )
            , ( "padding", "0px " )
            , ( "border", "0" )
            , ( "outline", "none" )
            ]
         ]
            ++ buttonAttrs
        )
        [ i
            ([ class "material-icons"
             , style
                [ ( "font-size", "24px" )
                , ( "cursor", "pointer" )
                , ( "color", "rgba(0,0,0,.54)" )
                , ( "box-sizing", "border-box" )
                ]
             ]
                ++ iconAttrs
            )
            [ text name ]
        ]


view : String -> List (Attribute msg) -> Html msg
view name a =
    i
        ([ class "material-icons"
         , style
            [ ( "font-size", "24px" )
            , ( "color", "rgba(0,0,0,.54)" )
            , ( "box-sizing", "border-box" )
            , ( "border", "0" )
            , ( "outline", "none" )
            ]
         ]
            ++ a
        )
        [ text name ]
