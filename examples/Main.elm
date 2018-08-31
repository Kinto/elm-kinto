module Main exposing (main)

import Browser
import Model exposing (Flags, Model, Msg)
import View exposing (view)



-- Todo & ideas
-- - Remove hardcoded auth
-- - New record form + HTTP POST
-- - Refresh button
-- - Styling


main : Program Flags Model Msg
main =
    Browser.element
        { init = Model.init
        , view = view
        , update = Model.update
        , subscriptions = Model.subscriptions
        }
