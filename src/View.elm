module View exposing (view)

import Time exposing (Time)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Utils exposing (timeAgo)
import Model exposing (Model, Record, Records, Msg(..))
import Form
import Dict


formatLastModified : Int -> Time -> String
formatLastModified timestamp currentTime =
    timeAgo (toFloat timestamp) currentTime


iconBtn : String -> Msg -> Html Msg
iconBtn icon action =
    button [ class "btn btn-xs btn-default", onClick action ]
        [ i [ class ("glyphicon glyphicon-" ++ icon) ] [] ]


recordRow : Time -> Record -> Html Msg
recordRow currentTime { id, title, description, last_modified } =
    tr []
        [ td [] [ text id ]
        , td [] [ text (Maybe.withDefault "[empty]" title) ]
        , td [] [ text (Maybe.withDefault "[empty]" description) ]
        , td [] [ text (formatLastModified last_modified currentTime) ]
        , td []
            [ iconBtn "edit" (EditRecord id)
            , text " "
            , iconBtn "trash" (DeleteRecord id)
            ]
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
                , th [] []
                ]
            ]
        , tbody [] (List.map (recordRow currentTime) records)
        ]


errorNotif : Maybe String -> Html Msg
errorNotif error =
    case error of
        Nothing ->
            text ""

        Just message ->
            div [ class "alert alert-danger" ] [ text ("Error: " ++ message) ]


sortedRecords : Records -> List Record
sortedRecords records =
    Dict.values records
        |> List.sortBy .last_modified
        |> List.reverse


view : Model -> Html Msg
view { error, records, formData, currentTime } =
    div [ class "container" ]
        [ h1 [] [ text "Kinto Elm :-)" ]
        , errorNotif error
        , recordsList (sortedRecords records) currentTime
        , Html.App.map FormMsg (Form.view formData)
        ]
