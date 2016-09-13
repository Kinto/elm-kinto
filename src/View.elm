module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Model exposing (Model, Record, Msg)
import Form exposing (recordForm)


recordRow : Record -> Html Msg
recordRow { id, title, description } =
    tr []
        [ td [] [ text id ]
        , td [] [ text (Maybe.withDefault "[empty]" title) ]
        , td [] [ text (Maybe.withDefault "[empty]" description)  ]
        ]


recordsList : List Record -> Html Msg
recordsList records =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "id" ]
                , th [] [ text "title" ]
                , th [] [ text "description" ]
                ]
            ]
        , tbody [] (List.map recordRow records)
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
view { error, errorMsg, records, formData } =
    div [ class "container" ]
        [ h1 [] [ text "Kinto Elm :-)" ]
        , errorNotif error errorMsg
        , recordsList records
        , recordForm { title = "", description = "" }
          --, stylesheet
        ]
