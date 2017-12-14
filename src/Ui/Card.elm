module Ui.Card
    exposing
        ( view
        , Config
        )

import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Events
import Ui.Options as Options exposing (Style, Property, cs, css, styled, when)
import Ui.Typography as Typography
import Icons.Icon as Icon
import Ui.Elevation as Elevation


type alias Config msg =
    { open : Bool
    , title : String
    , toMsg : msg
    }


view : Config a -> List (Html a) -> Html a
view { open, toMsg, title } nodes =
    let
        transformValue =
            if open then
                ("rotate(90deg)")
            else
                ("rotate(270deg)")
    in
        styled div
            [ Elevation.z1, cs "block", css "margin-top" "24px" ]
            [ styled div
                [ css "display" "flex"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                ]
                [ styled div
                    [ Typography.headline ]
                    [ text title ]
                , Icon.asButton "chevron_left"
                    [ Html.style [ ( "transform", transformValue ) ]
                    , Events.onClick toMsg
                    ]
                    []
                ]
            , styled div
                [ css "margin-top" "12px" |> when open ]
                (if open then
                    nodes
                 else
                    []
                )
            ]
