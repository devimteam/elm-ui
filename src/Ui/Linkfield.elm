module Ui.Linkfield exposing (..)

import Html exposing (Html, div, a, text, label)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD
import Html.Attributes
import Html.Attributes exposing (style)


type alias Config =
    { linkText : String
    , nonLinkText : String
    , label : String
    , width : Int
    , fullWidth : Bool
    , asTitle : Bool
    }


defaultConfig : Config
defaultConfig =
    { label = "Платежи"
    , linkText = "Есть"
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


view : Config -> List (Html.Attribute msg) -> Bool -> Html msg
view config linkAttributes showLink =
    let
        linkfieldStyle =
            [ ( "margin", "14px 0 8px 0" )
            , ( "width", getWidth config )
            ]

        labelStyle =
            [ ( "color", "rgba(0, 0, 0, 0.5)" )
            , ( "font-size", "12px" )
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
                    [ text config.linkText ]
            else
                text config.nonLinkText
    in
        div [ style linkfieldStyle ]
            [ label [ style labelStyle ] [ text config.label ]
            , div [ style bodyStyle ] [ body ]
            ]
