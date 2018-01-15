# elm-kinto client

[![Build Status](https://travis-ci.org/Kinto/elm-kinto.svg?branch=master)](https://travis-ci.org/Kinto/elm-kinto)

[Kinto](http://www.kinto-storage.org/) client for [elm](http://elm-lang.org/).

**Need help? Join the #kinto channel on the [freenode](https://freenode.net/)
irc server, or in the
[kinto-storage Slack](https://slack.kinto-storage.org/)!**


> Thanks to @luke_dot_js and his
> [elm-http-builder](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/)
> package which this library uses, and which was a huge inspiration for the
> design (and even for the README ;)

## Example

You can find some full examples in `src/examples`.

```elm
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



--

type Msg
    = TodoAdded (Result Kinto.Error Todo)
    | TodosFetched (Result Kinto.Error (Kinto.pager Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodoAdded (Ok todo) ->
            ( { model | todos = todo :: model.todos }, Cmd.none )

        TodosFetched (Err err) ->
            let
                _ =
                    Debug.log "Error while creating the todo" err
            in
                ( model, Cmd.none )

        TodosFetched (Ok todoPager) ->
            ( { model | todos = todoPager.objects }, Cmd.none )

        TodosFetched (Err err) ->
            let
                _ =
                    Debug.log "Error while getting the list of todos" err
            in
                ( model, Cmd.none )



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
        |> Kinto.sort [ "title", "description" ]
        |> Kinto.send TodosFetched
```

## Contributing

We're happy to receive any feedback and ideas for about additional features.
Any input and pull requests are very welcome and encouraged. If you'd like to
help or have ideas, get in touch with us on irc, slack, email, github or any
other mean listed on the [kinto webpage](http://www.kinto-storage.org/)!
