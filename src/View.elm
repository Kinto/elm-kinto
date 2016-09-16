module View exposing (view)

import Time exposing (Time)
import Html exposing (..)
import Html.App
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Utils exposing (timeAgo)
import Model exposing (Model, Record, Msg(..))
import Form


formatLastModified : Int -> Time -> String
formatLastModified timestamp currentTime =
    timeAgo (toFloat timestamp) currentTime


recordRow : Time -> Record -> Html Msg
recordRow currentTime { id, title, description, last_modified } =
    tr []
        [ td [] [ text id ]
        , td [] [ text (Maybe.withDefault "[empty]" title) ]
        , td [] [ text (Maybe.withDefault "[empty]" description) ]
        , td [] [ text (formatLastModified last_modified currentTime) ]
        ]


recordsList : List Record -> Time -> Html Msg
recordsList records currentTime =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "id" ]
                , th [] [ text "title" ]
                , th [] [ text "description" ]
                , th [] [ text "last_modified" ]
                ]
            ]
        , tbody [] (List.map (recordRow currentTime) records)
        ]


errorNotif : Bool -> String -> Html Msg
errorNotif error errorMsg =
    if error == True then
        div [ class "alert alert-danger" ] [ text ("Error: " ++ errorMsg) ]
    else
        div [] [ text "" ]



--stylesheet : Html Msg
--stylesheet =
--    -- Silly hack to load bootstrap because other ways are clumsy.
--    let
--        tag =
--            "link"
--        attrs =
--            [ attribute "rel" "stylesheet"
--            , attribute "property" "stylesheet"
--            , attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
--            ]
--        children =
--            []
--    in
--        node tag attrs children


view : Model -> Html Msg
view { error, errorMsg, records, formData, currentTime } =
    div [ class "container" ]
        [ h1 [] [ text "Kinto Elm :-)" ]
        , errorNotif error errorMsg
        , recordsList records currentTime
        , Html.App.map FormMsg (Form.view formData)
          -- , stylesheet
        ]
