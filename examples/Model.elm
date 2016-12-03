module Model
    exposing
        ( init
        , update
        , subscriptions
        , FormData
        , Model
        , Msg(..)
        , Record
        , Sort(..)
        )

import Time
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto


type alias RecordId =
    String


type alias Record =
    { id : RecordId
    , title : Maybe String
    , description : Maybe String
    , last_modified : Int
    }


type alias FormData =
    { id : Maybe String
    , title : String
    , description : String
    }


type alias Model =
    { error : Maybe String
    , records : List Record
    , formData : FormData
    , currentTime : Time.Time
    , sort : Sort
    , limit : Maybe Int
    }


type Sort
    = Asc String
    | Desc String


type Msg
    = NoOp
      -- Use by TimeAgo to display human friendly timestamps
    | Tick Time.Time
      -- Kinto API requests
    | FetchRecordResponse (Result Kinto.Error Record)
    | FetchRecords
    | FetchRecordsResponse (Result Kinto.Error (List Record))
    | CreateRecordResponse (Result Kinto.Error Record)
    | EditRecord RecordId
    | EditRecordResponse (Result Kinto.Error Record)
    | DeleteRecord RecordId
    | DeleteRecordResponse (Result Kinto.Error Record)
      -- Form
    | UpdateFormTitle String
    | UpdateFormDescription String
    | Submit
      -- Sorting
    | SortByColumn String
      -- Limiting
    | NewLimit String


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch [ fetchRecordList (Desc "last_modified") (Just 30) ]
    )


initialFormData : FormData
initialFormData =
    FormData Nothing "" ""


initialModel : Model
initialModel =
    { error = Nothing
    , records = []
    , formData = initialFormData
    , currentTime = 0
    , sort = Desc "last_modified"
    , limit = Just 30
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        FetchRecords ->
            ( { model | records = [], error = Nothing }
            , fetchRecordList model.sort model.limit
            )

        FetchRecordResponse (Ok record) ->
            ( { model
                | formData = recordToFormData record
                , error = Nothing
              }
            , Cmd.none
            )

        FetchRecordResponse (Err error) ->
            model |> updateError error

        FetchRecordsResponse (Ok recordList) ->
            ( { model
                | records = recordList
                , error = Nothing
              }
            , Cmd.none
            )

        FetchRecordsResponse (Err error) ->
            model |> updateError error

        CreateRecordResponse (Ok _) ->
            ( { model | formData = initialFormData }
            , fetchRecordList model.sort model.limit
            )

        CreateRecordResponse (Err error) ->
            model |> updateError error

        EditRecord recordId ->
            ( model, fetchRecord recordId )

        EditRecordResponse (Ok _) ->
            ( model, fetchRecordList model.sort model.limit )

        EditRecordResponse (Err error) ->
            model |> updateError error

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordResponse (Ok record) ->
            ( { model
                | records = removeRecordFromList record model.records
                , error = Nothing
              }
            , fetchRecordList model.sort model.limit
            )

        DeleteRecordResponse (Err error) ->
            model |> updateError error

        UpdateFormTitle title ->
            let
                formData =
                    model.formData

                updated =
                    { formData | title = title }
            in
                ( { model
                    | formData = updated
                    , records = updateRecordInList updated model.records
                  }
                , Cmd.none
                )

        UpdateFormDescription description ->
            let
                formData =
                    model.formData

                updated =
                    { formData | description = description }
            in
                ( { model
                    | formData = updated
                    , records = updateRecordInList updated model.records
                  }
                , Cmd.none
                )

        Submit ->
            ( { model | formData = initialFormData }, sendFormData model.formData )

        SortByColumn column ->
            let
                sort =
                    case model.sort of
                        Asc sortedColumn ->
                            if sortedColumn == column then
                                (Desc sortedColumn)
                            else
                                (Asc column)

                        Desc sortedColumn ->
                            if sortedColumn == column then
                                (Asc sortedColumn)
                            else
                                (Asc column)
            in
                ( { model | sort = sort }, fetchRecordList sort model.limit )

        NewLimit newLimit ->
            case String.toInt newLimit of
                Ok limit ->
                    ( { model | limit = Just limit }
                    , fetchRecordList model.sort (Just limit)
                    )

                Err _ ->
                    ( { model | limit = Nothing }
                    , fetchRecordList model.sort Nothing
                    )


updateError : error -> Model -> ( Model, Cmd Msg )
updateError error model =
    ( { model | error = Just <| toString error }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick



-- Helpers


recordToFormData : Record -> FormData
recordToFormData { id, title, description } =
    FormData
        (Just id)
        (Maybe.withDefault "" title)
        (Maybe.withDefault "" description)


encodeFormData : FormData -> Encode.Value
encodeFormData { title, description } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        ]


removeRecordFromList : Record -> List Record -> List Record
removeRecordFromList { id } records =
    List.filter (\record -> record.id /= id) records


updateRecordInList : FormData -> List Record -> List Record
updateRecordInList formData records =
    -- This enables live reflecting ongoing form updates in the records list
    case formData.id of
        Nothing ->
            records

        Just id ->
            List.map
                (\record ->
                    if record.id == id then
                        updateRecord formData record
                    else
                        record
                )
                records


updateRecord : FormData -> Record -> Record
updateRecord formData record =
    { record
        | title = Just formData.title
        , description = Just formData.description
    }



-- Kinto client configuration


client : Kinto.Client
client =
    Kinto.client
        "https://kinto.dev.mozaws.net/v1/"
        (Kinto.Basic "test" "test")


recordResource : Kinto.Resource Record
recordResource =
    Kinto.recordResource "default" "test-items" decodeRecord


decodeRecord : Decode.Decoder Record
decodeRecord =
    (Decode.map4 Record
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "last_modified" Decode.int)
    )



-- Kinto API calls


fetchRecord : RecordId -> Cmd Msg
fetchRecord recordId =
    client
        |> Kinto.get recordResource recordId
        |> Kinto.send FetchRecordResponse


fetchRecordList : Sort -> Maybe Int -> Cmd Msg
fetchRecordList sort limit =
    let
        sortColumn =
            case sort of
                Asc column ->
                    column

                Desc column ->
                    "-" ++ column

        limiter builder =
            case limit of
                Just limit ->
                    builder
                        |> Kinto.limit limit

                Nothing ->
                    builder
    in
        client
            |> Kinto.getList recordResource
            |> Kinto.sortBy [ sortColumn ]
            |> limiter
            |> Kinto.send FetchRecordsResponse


deleteRecord : RecordId -> Cmd Msg
deleteRecord recordId =
    client
        |> Kinto.delete recordResource recordId
        |> Kinto.send DeleteRecordResponse


sendFormData : FormData -> Cmd Msg
sendFormData formData =
    let
        data =
            encodeFormData formData
    in
        case formData.id of
            Nothing ->
                client
                    |> Kinto.create recordResource data
                    |> Kinto.send CreateRecordResponse

            Just recordId ->
                client
                    |> Kinto.update recordResource recordId data
                    |> Kinto.send EditRecordResponse
