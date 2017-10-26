module Utils.General exposing (..)

import FormatNumber.Locales exposing (Locale)
import Date
import Date.Format
import Regex exposing (HowMany(..))
import FormatNumber


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
            count % 100 |> abs

        n10 =
            count % 10 |> abs
    in
        if n >= 5 && n <= 20 then
            five
        else if n10 == 1 then
            one
        else if n10 >= 2 && n10 <= 4 then
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


formatPhone : String -> String
formatPhone phone =
    let
        combineParts : List String -> String
        combineParts list =
            (list |> getNthWithDefault 0 "")
                ++ " ("
                ++ (list |> getNthWithDefault 1 "")
                ++ ") "
                ++ (List.drop 2 list |> String.join "-")
    in
        Regex.replace
            All
            (Regex.regex "(^\\+\\d)(\\d{3})(\\d{3})(\\d{2})(.*)")
            (\match ->
                match.submatches
                    |> List.map (\submatch -> Maybe.withDefault "" submatch)
                    |> combineParts
            )
            phone


getNth : Int -> List a -> Maybe a
getNth n list =
    List.head (List.drop n list)


getNthWithDefault : Int -> a -> List a -> a
getNthWithDefault n default list =
    List.drop n list |> List.head |> Maybe.withDefault default


formatMoney : Float -> String
formatMoney amount =
    FormatNumber.format rusLocale amount ++ " ₽"
