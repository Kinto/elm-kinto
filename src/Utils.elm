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
            "less than a minute ago"
        else if seconds <= 65 then
            "about a minute ago"
        else if minutes <= 55 then
            plural "minute" minutes
        else if minutes <= 65 then
            "about an hour ago"
        else if hours <= 22 then
            plural "hour" hours
        else if hours <= 26 then
            "about a day ago"
        else if days >= 6 && days <= 8 then
            "about a week ago"
        else if days >= 14 && days <= 15 then
            "about 2 weeks ago"
        else if days >= 20 && days <= 22 then
            "about 3 weeks ago"
        else if days <= 27 then
            plural "day" days
        else if days <= 33 then
            "about a month ago"
        else if months >= 11.25 && months <= 12.75 then
            "about a year ago"
        else if months < 12 then
            plural "month" months
        else
            plural "year" years


plural : String -> Float -> String
plural unit amount =
    let
        roundAmount =
            round amount

        humanAmount =
            if roundAmount == 1 then
                "one"
            else
                toString roundAmount

        unitPlural =
            if roundAmount > 1 then
                unit ++ "s"
            else
                unit
    in
        humanAmount ++ " " ++ unitPlural ++ " ago"
