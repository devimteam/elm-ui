module Utils.General exposing (..)

import FormatNumber.Locales exposing (Locale)
import Date
import Date.Format


type Plural
    = Plural String String String


rusLocale : Locale
rusLocale =
    Locale 0 " " "," "-" ""


rusLocale1 : Locale
rusLocale1 =
    Locale 2 " " "," "-" ""


pluralize : Plural -> Int -> String
pluralize (Plural one two five) count =
    let
        n =
            count % 100

        n10 =
            count % 10
    in
        if n >= 5 && n <= 20 then
            five
        else if n10 == 1 then
            one
        else if n10 >= 2 && n <= 4 then
            two
        else
            five


formatDate : String -> String
formatDate dateString =
    let
        date =
            Date.fromString dateString

        result =
            case date of
                Ok d ->
                    Date.Format.format "%d.%m.%Y" d

                Err err ->
                    ""
    in
        result


formatTime : String -> String
formatTime dateString =
    let
        date =
            Date.fromString dateString

        result =
            case date of
                Ok d ->
                    Date.Format.format "%H:%M:%S" d

                Err err ->
                    ""
    in
        result
