module Main exposing (..)

import Html
import Model exposing (Model, Msg)
import View exposing (view)


-- Todo & ideas
-- - Remove hardcoded auth
-- - New record form + HTTP POST
-- - Refresh button
-- - Styling


main : Program Never Model Msg
main =
    Html.program
        { init = Model.init
        , view = view
        , update = Model.update
        , subscriptions = Model.subscriptions
        }
