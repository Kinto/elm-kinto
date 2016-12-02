module View exposing (view)

import Time exposing (Time)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Attributes exposing (id, for, attribute, class, type_, value)
import Utils exposing (timeAgo)
import Model exposing (Model, Record, Msg(..))
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


view : Model -> Html Msg
view { error, records, formData, currentTime } =
    div [ class "container" ]
        [ h1 [] [ text "Kinto Elm :-)" ]
        , errorNotif error
        , recordsList records currentTime
        , formView formData
        ]


formVerb : Model.FormData -> String
formVerb { id } =
    case id of
        Nothing ->
            "Create"

        Just _ ->
            "Update"


formTitle : Model.FormData -> String
formTitle model =
    (formVerb model)
        ++ " "
        ++ (Maybe.withDefault "" model.id)
        |> String.trim


formView : Model.FormData -> Html Msg
formView formData =
    form [ onSubmit Submit ]
        [ fieldset []
            [ legend [] [ text (formTitle formData) ]
            , div [ class "form-group" ]
                [ label [ for "title" ] [ text "Title" ]
                , input
                    [ id "title"
                    , type_ "text"
                    , class "form-control"
                    , value formData.title
                    , onInput UpdateFormTitle
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [ for "description" ] [ text "Description" ]
                , textarea
                    [ id "description"
                    , class "form-control"
                    , value formData.description
                    , onInput UpdateFormDescription
                    ]
                    []
                ]
            , div []
                [ button [ type_ "submit", class "btn btn-default" ]
                    [ text (formVerb formData) ]
                ]
            ]
        ]
