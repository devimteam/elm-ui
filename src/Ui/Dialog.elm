module Ui.Dialog
    exposing
        ( view
        , open
        , header
        , title
        , body
        , scrollable
        , footer
        , acceptButton
        , cancelButton
          -- , openOn
          -- , closeOn
        )

import Html exposing (..)
import Html.Attributes as Html
import Ui.Options as Options exposing (Style, Property, cs)
import Ui.Internal.Options as Internal


{-| Dialog header
-}
header : List (Style a) -> List (Html a) -> Html a
header options =
    Options.header (cs "mdc-dialog__header" :: options)


{-| Dialog title
-}
title : List (Style a) -> List (Html a) -> Html a
title options =
    Options.div (cs "mdc-dialog__header__title" :: options)


{-| Dialog body
-}
body : List (Style a) -> List (Html a) -> Html a
body options =
    Options.div (cs "mdc-dialog__body" :: options)


{-| Make dialog body scrollable
-}
scrollable : Style a
scrollable =
    cs "mdc-dialog__body--scrollable"


{-| Generate an actions content block
-}
footer : List (Style a) -> List (Html a) -> Html a
footer options =
    Options.div (cs "mdc-dialog__footer" :: options)


{-| Dialog's accept button
-}
acceptButton :
    (List (Options.Property s a) -> List (Html a) -> Html a)
    -> List (Options.Property s a)
    -> List (Html a)
    -> Html a
acceptButton button options =
    button (cs "mdc-dialog__footer__button" :: cs "mdc-dialog__footer__button--accept" :: options)


{-| Dialog's cancel button
-}
cancelButton :
    (List (Options.Property s a) -> List (Html a) -> Html a)
    -> List (Options.Property s a)
    -> List (Html a)
    -> Html a
cancelButton button options =
    button (cs "mdc-dialog__footer__button" :: cs "mdc-dialog__footer__button--cancel" :: options)


theDialog : String
theDialog =
    "elm-mdc-singleton-dialog"


view : List (Style a) -> List (Html a) -> Html a
view styling nodes =
    Options.styled_ (Html.node "aside")
        (cs "mdc-dialog" :: styling)
        [ Html.id theDialog ]
        [ Html.div [ Html.class "mdc-dialog__surface" ] nodes
        , Html.div [ Html.class "mdc-dialog__backdrop" ] []
        ]


open : Style a
open =
    cs "mdc-dialog--open"
