module Form exposing (view, Model, init, update, Msg(CreateRecord))

import Html exposing (..)
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Html.Events exposing (onInput, onSubmit)


type alias Model =
    { title : String
    , description : String
    }


init : Model
init =
    { title = ""
    , description = ""
    }



-- Update


type Msg
    = UpdateFormTitle String
    | UpdateFormDescription String
    | CreateRecord


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFormTitle title ->
            { model | title = title }

        UpdateFormDescription description ->
            { model | description = description }

        CreateRecord ->
            model



-- View


view : Model -> Html Msg
view { title, description } =
    form [ onSubmit CreateRecord ]
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
