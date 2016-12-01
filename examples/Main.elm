module Main exposing (..)

import Html
import Model exposing (..)
import View exposing (view)


-- Todo & ideas
-- - Remove hardcoded auth
-- - New record form + HTTP POST
-- - Refresh button
-- - Styling


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
