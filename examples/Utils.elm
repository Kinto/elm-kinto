module Utils exposing (timeAgo)

import Time exposing (Time)


timeAgo : Time -> Time -> String
timeAgo time now =
    -- Hint: Time is a Float
    let
        seconds =
            (now - time) / 1000

        minutes =
            seconds / 60

        hours =
            minutes / 60

        days =
            hours / 24

        months =
            days / (365.25 / 12)

        years =
            days / 365.25
    in
        if seconds < 10 then
            "a few seconds ago"
        else if seconds < 55 then
            plural "second" seconds
        else if seconds |> within 55 65 then
            "about a minute ago"
        else if minutes < 55 then
            plural "minute" minutes
        else if minutes |> within 55 65 then
            "about an hour ago"
        else if hours < 22 then
            plural "hour" hours
        else if hours |> within 22 26 then
            "about a day ago"
        else if days |> within 6 8 then
            "about a week ago"
        else if days |> within 14 16 then
            "about 2 weeks ago"
        else if days |> within 20 22 then
            "about 3 weeks ago"
        else if days < 27 then
            plural "day" days
        else if days |> within 27 33 then
            "about a month ago"
        else if months < 11 then
            plural "month" months
        else if months |> within 11 13 then
            "about a year ago"
        else
            plural "year" years


within : Float -> Float -> Float -> Bool
within low high value =
    value >= low && value <= high


plural : String -> Float -> String
plural unit amount =
    let
        roundAmount =
            round amount

        unitPlural =
            if roundAmount > 1 then
                unit ++ "s"
            else
                unit
    in
        (toString roundAmount) ++ " " ++ unitPlural ++ " ago"
