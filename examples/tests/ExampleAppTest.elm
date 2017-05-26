module ExampleAppTest exposing (all)

import Expect
import Test exposing (..)
import Utils exposing (timeAgo)


timeAgoTest : String -> Float -> String -> Test
timeAgoTest description diff result =
    let
        now =
            1473870908000

        current =
            now - diff

        subject =
            timeAgo current now
    in
        test description <| \() -> Expect.equal subject result


all : Test
all =
    describe "all tests"
        [ describe "Date.TimeAgo"
            [ timeAgoTest "<10s diff" (seconds 4) "a few seconds ago"
            , timeAgoTest "<1mn diff" (seconds 12) "12 seconds ago"
            , timeAgoTest "=1mn- diff" (seconds 55) "about a minute ago"
            , timeAgoTest "=1mn- diff-2" (seconds 58) "about a minute ago"
            , timeAgoTest "=1mn= diff" (seconds 60) "about a minute ago"
            , timeAgoTest "=1mn+ diff" (seconds 62) "about a minute ago"
            , timeAgoTest "=1mn+ diff-2" (seconds 65) "about a minute ago"
            , timeAgoTest "<1h diff" (minutes 12) "12 minutes ago"
            , timeAgoTest "<1h diff-2" (minutes 30) "30 minutes ago"
            , timeAgoTest "<1h diff-3" (minutes 48) "48 minutes ago"
            , timeAgoTest "=1h- diff" (minutes 50) "50 minutes ago"
            , timeAgoTest "=1h- diff-2" (minutes 58) "about an hour ago"
            , timeAgoTest "=1h= diff" (minutes 60) "about an hour ago"
            , timeAgoTest "=1h+ diff" (minutes 62) "about an hour ago"
            , timeAgoTest "<1d diff" (hours 12) "12 hours ago"
            , timeAgoTest "=1d- diff-" (hours 23) "about a day ago"
            , timeAgoTest "=1d= diff" (hours 24) "about a day ago"
            , timeAgoTest "=1d+ diff+" (hours 25) "about a day ago"
            , timeAgoTest "~=1week diff" (days 7) "about a week ago"
            , timeAgoTest "~=2weeks diff" (days 14) "about 2 weeks ago"
            , timeAgoTest "~=3weeks diff" (days 21) "about 3 weeks ago"
            , timeAgoTest "<1month diff" (days 12) "12 days ago"
            , timeAgoTest "=1month= diff" (days 31) "about a month ago"
            , timeAgoTest "<1month diff, week match" (days 14) "about 2 weeks ago"
            , timeAgoTest "<1year diff" (months 6) "6 months ago"
            , timeAgoTest "<1year diff-2" (months 10) "10 months ago"
            , timeAgoTest "=1year- diff" (days 350) "about a year ago"
            , timeAgoTest "=1year= diff" (days 365) "about a year ago"
            , timeAgoTest "=1year+ diff" (days 380) "about a year ago"
            , timeAgoTest ">1 year" (years 12) "12 years ago"
            ]
        ]


seconds : Float -> Float
seconds x =
    x * 1000


minutes : Float -> Float
minutes x =
    x * (seconds 60)


hours : Float -> Float
hours x =
    x * (minutes 60)


days : Float -> Float
days x =
    x * (hours 24)


months : Float -> Float
months x =
    x * (days (365.25 / 12))


years : Float -> Float
years x =
    x * (days 365.25)
