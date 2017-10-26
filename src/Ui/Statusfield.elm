module Ui.Statusfield exposing (..)

import Html exposing (Html, div, a, text, label)
import Html.Events exposing (onWithOptions)
import Json.Decode as JD
import Html.Attributes
import Html.Attributes exposing (style)


type alias Config =
    { label : String
    , width : Int
    , fullWidth : Bool
    }


type alias Status =
    String


type alias StatusData =
    { backgroundColor : String
    , text : String
    }


defaultConfig : Config
defaultConfig =
    { label = "Статус займа"
    , fullWidth = False
    , width = 136
    }


getWidth : Config -> String
getWidth config =
    if config.fullWidth then
        "100%"
    else
        toString config.width |> flip (++) "px"


getStatusData : Status -> StatusData
getStatusData status =
    case status of
        "PROCESSED" ->
            { backgroundColor = "#3f51b4", text = "Выдано" }

        _ ->
            { backgroundColor = "#78909c", text = "?" }


view : Config -> Status -> Html msg
view config status =
    let
        statusData =
            getStatusData status

        indicatorStyle =
            [ ( "border-radius", "100%" )
            , ( "width", "12px" )
            , ( "height", "12px" )
            , ( "background-color", statusData.backgroundColor )
            , ( "margin-right", "8px" )
            ]

        statusfieldStyle =
            [ ( "margin", "14px 0 8px 0" )
            , ( "width", getWidth config )
            ]

        labelStyle =
            [ ( "color", "rgba(0, 0, 0, 0.5)" )
            , ( "font-size", "12px" )
            ]

        bodyStyle =
            [ ( "padding-top", "4px" )
            , ( "font-size", "16px" )
            , ( "display", "flex" )
            , ( "alignItems", "center" )
            ]

        body =
            div [ style bodyStyle ]
                [ div [ style indicatorStyle ] []
                , div [] [ text statusData.text ]
                ]
    in
        div [ style statusfieldStyle ]
            [ label [ style labelStyle ] [ text config.label ]
            , body
            ]
