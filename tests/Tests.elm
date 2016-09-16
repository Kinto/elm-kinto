module Tests exposing (..)

import Test exposing (..)
import Expect
import Utils exposing (timeAgo)


timeAgoTest : String -> Float -> String -> Test
timeAgoTest description diff result =
    let
        now =
            1473870908000

        current =
            now - (diff * 1000)

        subject =
            timeAgo current now
    in
        test description <| \() -> Expect.equal subject result


all : Test
all =
    describe "Date.TimeAgo"
        [ timeAgoTest "<1mn diff" 30 "less than a minute ago"
        , timeAgoTest "=1mn diff" 60 "a minute ago"
        , timeAgoTest "<1h diff" (60 * 30) "30 minutes ago"
        , timeAgoTest "=1h diff" (60 * 60) "an hour ago"
        , timeAgoTest "<1d diff" (60 * 60 * 12) "12 hours ago"
        , timeAgoTest "=1d diff" (60 * 60 * 24) "a day ago"
        , timeAgoTest "<1month diff" (60 * 60 * 24 * 12) "12 days ago"
        , timeAgoTest "=1month diff" (60 * 60 * 24 * 31) "a month ago"
        , timeAgoTest "<1month diff, week match" (60 * 60 * 24 * 14) "2 weeks ago"
        , timeAgoTest "<1year diff" (60 * 60 * 24 * 30 * 6) "6 months ago"
        , timeAgoTest ">1 year" (60 * 60 * 24 * 365 * 12) "12 years ago"
        ]
