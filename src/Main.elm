module Main exposing (..)

import Html.App as Html
import Model exposing (..)
import View exposing (view)


-- Todo & ideas
-- - Remove hardcoded auth
-- - New record form + HTTP POST
-- - Refresh button
-- - Styling


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
