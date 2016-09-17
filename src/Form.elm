module Form exposing (view, Model, init, update, Msg, OutMsg(..))

import Html exposing (..)
import Html.Attributes exposing (id, for, attribute, class, type', value)
import Html.Events exposing (onInput, onSubmit)


type alias Model =
    { id : Maybe String
    , title : String
    , description : String
    }


init : Model
init =
    { id = Nothing
    , title = ""
    , description = ""
    }



-- Update


type Msg
    = UpdateFormTitle String
    | UpdateFormDescription String
    | Submit (Maybe String)


type OutMsg
    = FormSubmitted Model


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    case msg of
        UpdateFormTitle title ->
            ( { model | title = title }, Nothing )

        UpdateFormDescription description ->
            ( { model | description = description }, Nothing )

        Submit id ->
            ( model
            , Just (FormSubmitted model)
            )



-- View


btnText : Model -> String
btnText { id } =
    case id of
        Nothing ->
            "Create"

        Just _ ->
            "Update"


view : Model -> Html Msg
view model =
    form [ onSubmit (Submit model.id) ]
        [ div [ class "form-group" ]
            [ label [ for "title" ] [ text "Title" ]
            , input
                [ id "title"
                , type' "text"
                , class "form-control"
                , value model.title
                , onInput UpdateFormTitle
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "description" ] [ text "Description" ]
            , textarea
                [ id "description"
                , class "form-control"
                , value model.description
                , onInput UpdateFormDescription
                ]
                []
            ]
        , div []
            [ button [ type' "submit", class "btn btn-default" ]
                [ text (btnText model) ]
            ]
        ]
