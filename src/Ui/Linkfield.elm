module Ui.Linkfield exposing (..)

import Html exposing (Html, div, a, text, label, Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD
import Html.Attributes exposing (style)
import Json.Decode as JD


type alias Config =
    { nonLinkText : String
    , label : String
    , width : Int
    , fullWidth : Bool
    , asTitle : Bool
    }


currencyConfig : String -> Config
currencyConfig label =
    { label = label
    , nonLinkText = ""
    , fullWidth = False
    , width = 200
    , asTitle = True
    }


defaultConfig : Config
defaultConfig =
    { label = "Платежи"
    , nonLinkText = "Нет"
    , fullWidth = False
    , width = 136
    , asTitle = False
    }


getWidth : Config -> String
getWidth config =
    if config.fullWidth then
        "100%"
    else
        toString config.width |> flip (++) "px"


getFontSize : Bool -> String
getFontSize asTitle =
    if asTitle then
        "34px"
    else
        "16px"


getMargin : Bool -> String
getMargin asTitle =
    if asTitle then
        "4px 0 8px 0"
    else
        "14px 0 8px 0"


view : String -> Config -> List (Html.Attribute msg) -> Bool -> Html msg
view value config linkAttributes showLink =
    let
        linkfieldStyle =
            [ ( "margin", getMargin config.asTitle )
            , ( "width", getWidth config )
            ]

        labelStyle =
            [ ( "color", "rgba(0, 0, 0, 0.5)" )
            , ( "font-size", "12px" )
            , ( "white-space", "nowrap" )
            ]

        linkStyle =
            [ ( "color", "#009ce1" )
            , ( "text-decoration", "underline" )
            , ( "cursor", "pointer" )
            ]

        bodyStyle =
            [ ( "padding-top", "4px" )
            , ( "font-size", getFontSize config.asTitle )
            ]

        body =
            if showLink then
                a
                    ([ style linkStyle ] ++ linkAttributes)
                    [ text value ]
            else
                text config.nonLinkText
    in
        div [ style linkfieldStyle ]
            [ label [ style labelStyle ] [ text config.label ]
            , div [ style bodyStyle ] [ body ]
            ]


onClick : msg -> Attribute msg
onClick m =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = True }
        (JD.succeed m)
