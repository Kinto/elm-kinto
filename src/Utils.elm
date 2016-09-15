module Utils exposing (timeAgo)

import Time exposing (Time)


timeAgo : Time -> Time -> String
timeAgo time now =
    let
        seconds =
            round ((now - time) / 1000)

        minutes =
            seconds // 60

        hours =
            minutes // 60

        days =
            hours // 24

        weeks =
            days // 7

        months =
            days // 30

        years =
            days // 365
    in
        if seconds < 60 then
            "less than a minute ago"
        else if minutes < 60 then
            plural "minute" minutes
        else if hours < 24 then
            plural "hour" hours
        else if weeks == 2 || weeks == 3 then
            plural "week" weeks
        else if days < 31 then
            plural "day" days
        else if months < 12 then
            plural "month" months
        else
            plural "year" years


plural : String -> Int -> String
plural unit amount =
    let
        unitPlural =
            if amount > 1 then
                unit ++ "s"
            else
                unit

        humanAmount =
            if amount == 1 then
                if unit == "hour" then
                    "an"
                else
                    "a"
            else
                toString amount
    in
        humanAmount ++ " " ++ unitPlural ++ " ago"
