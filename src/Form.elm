module Form exposing (recordForm)

import Html exposing (..)
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Html.Events exposing (onInput, onSubmit)
import Model exposing (FormData, Msg(..))


recordForm : FormData -> Html Msg
recordForm { title, description } =
    form [ onSubmit SubmitForm ]
        [ div [ class "form-group" ]
            [ label [ for "title" ] [ text "Title" ]
            , input
                [ id "title"
                , type' "text"
                , class "form-control"
                  --, value title
                , onInput UpdateFormTitle
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "description" ] [ text "Description" ]
            , textarea
                [ id "description"
                , class "form-control"
                , onInput UpdateFormDescription
                ]
                []
            ]
        , div []
            [ button [ type' "submit", class "btn btn-default" ]
                [ text "Create" ]
            ]
        ]
