module Tests exposing (..)

import Test exposing (..)
import Expect
import Utils exposing (timeAgo)


timeDiff : Float -> String
timeDiff diff =
    let
        now =
            1473870908000

        current =
            now - (diff * 1000)
    in
        timeAgo current now


all : Test
all =
    describe "Date.TimeAgo"
        [ test "<1mn diff" <|
            \() ->
                Expect.equal (timeDiff 30) "less than a minute ago"
        , test "<1h diff" <|
            \() ->
                Expect.equal (timeDiff (60 * 30)) "30 minutes ago"
        , test "~=1h diff" <|
            \() ->
                Expect.equal (timeDiff (60 * 60)) "an hour ago"
        , test "<1d diff" <|
            \() ->
                Expect.equal (timeDiff (60 * 60 * 12)) "12 hours ago"
        , test "<1month diff" <|
            \() ->
                Expect.equal (timeDiff (60 * 60 * 24 * 12)) "12 days ago"
        , test "<1month diff, week match" <|
            \() ->
                Expect.equal (timeDiff (60 * 60 * 24 * 14)) "2 weeks ago"
        , test "<1year diff" <|
            \() ->
                Expect.equal (timeDiff (60 * 60 * 24 * 30 * 6)) "6 months ago"
        , test ">1 year" <|
            \() ->
                Expect.equal (timeDiff (60 * 60 * 24 * 365 * 12)) "12 years ago"
        ]
