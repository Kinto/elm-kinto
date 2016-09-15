module Form exposing (view, Model, init, update, Msg, OutMsg(..))

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
    | Submit


type OutMsg
    = FormSubmitted Model


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    case msg of
        UpdateFormTitle title ->
            ( { model | title = title }, Nothing )

        UpdateFormDescription description ->
            ( { model | description = description }, Nothing )

        Submit ->
            ( init
              -- empty the fields on submission
            , Just (FormSubmitted model)
            )



-- View


view : Model -> Html Msg
view { title, description } =
    form [ onSubmit Submit ]
        [ div [ class "form-group" ]
            [ label [ for "title" ] [ text "Title" ]
            , input
                [ id "title"
                , type' "text"
                , class "form-control"
                , value title
                , onInput UpdateFormTitle
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "description" ] [ text "Description" ]
            , textarea
                [ id "description"
                , class "form-control"
                , value description
                , onInput UpdateFormDescription
                ]
                []
            ]
        , div []
            [ button [ type' "submit", class "btn btn-default" ]
                [ text "Create" ]
            ]
        ]
