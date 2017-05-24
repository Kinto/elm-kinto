module Example exposing (..)

import Html
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto


-- Define what our Kinto records will look like for a Todo list.


type alias Todo =
    { id : String
    , title : Maybe String
    , description : Maybe String
    , last_modified : Int
    }



-- And how to decode from a Kinto record to a Todo...


decodeTodo : Decode.Decoder Todo
decodeTodo =
    Decode.map4 Todo
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "last_modified" Decode.int)



-- And how to encode Todo data to send to Kinto.


encodeData : Maybe String -> Maybe String -> Encode.Value
encodeData title description =
    Encode.object
        [ ( "title", Encode.string (Maybe.withDefault "" title) )
        , ( "description", Encode.string (Maybe.withDefault "" description) )
        ]



-- Configure a Kinto client: the server url and authentication


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.dev.mozaws.net/v1/"
        (Kinto.Basic "test" "test")



-- Set up the resource we're interested in: in this case, Kinto records, stored
-- in the "test-items" collection, in the "default" bucket.
-- We give it the `decodeTodo` decoder so the data we get back from the Kinto
-- server will be properly decoded into our `Todo` record.


recordResource : Kinto.Resource Todo
recordResource =
    Kinto.recordResource "default" "test-items" decodeTodo



-- Add a new Todo


addTodo : Maybe String -> Maybe String -> Cmd Msg
addTodo title description =
    let
        data =
            encodeData title description
    in
        client
            |> Kinto.create recordResource data
            |> Kinto.send TodoAdded



-- Get all the Todos


getTodoList : Cmd Msg
getTodoList =
    client
        |> Kinto.getList recordResource
        |> Kinto.sortBy [ "title", "description" ]
        |> Kinto.send TodosFetched



-- Tie it all together


type alias Model =
    { todos : List Todo
    , title : Maybe String
    , description : Maybe String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    Model [] Nothing Nothing Nothing
        ! [ addTodo (Just "test") (Just "description")
          , getTodoList
          ]


type Msg
    = TodoAdded (Result Kinto.Error Todo)
    | TodosFetched (Result Kinto.Error (Kinto.Pager Todo))


view : Model -> Html.Html Msg
view model =
    Html.text <| toString model.todos


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TodoAdded (Ok todo) ->
            { model | error = Nothing } ! []

        TodoAdded (Err error) ->
            { model | error = Just <| toString error } ! []

        TodosFetched (Ok pager) ->
            { model | error = Nothing, todos = pager.objects } ! []

        TodosFetched (Err error) ->
            { model | error = Just <| toString error } ! []


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
