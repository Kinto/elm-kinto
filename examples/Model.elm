module Model exposing (..)

import Time exposing (Time, second)
import Json.Decode as Decode exposing (Decoder, string, at, list, map4, field, maybe, int, Value, decodeValue)
import Json.Encode as Encode
import Form
import Dict
import Kinto


-- TODO:
-- - Expose only what's necessary
-- MODEL and TYPES


type alias RecordId =
    String


type alias Record =
    { id : RecordId
    , title : Maybe String
    , description : Maybe String
    , last_modified : Int
    }


type alias Records =
    Dict.Dict RecordId Record


type alias Model =
    { error : Maybe String
    , records : List Record
    , formData : Form.Model
    , currentTime : Time
    }


type Msg
    = NoOp
    | Tick Time
    | FetchRecordResponse (Result Kinto.Error Record)
    | FetchRecords
    | FetchRecordsResponse (Result Kinto.Error (List Record))
    | FormMsg Form.Msg
    | CreateRecordResponse (Result Kinto.Error Record)
    | EditRecord RecordId
    | EditRecordResponse (Result Kinto.Error Record)
    | DeleteRecord RecordId
    | DeleteRecordResponse (Result Kinto.Error Record)


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch [ fetchRecordList ]
    )


initialModel : Model
initialModel =
    { error = Nothing
    , records = []
    , formData = Form.init
    , currentTime = 0
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
            ( { model | records = [], error = Nothing }, fetchRecordList )

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

        FormMsg subMsg ->
            let
                ( updated, formMsg ) =
                    Form.update subMsg model.formData
            in
                case formMsg of
                    Nothing ->
                        ( { model
                            | formData = updated
                            , records = updateRecordInList updated model.records
                          }
                        , Cmd.none
                        )

                    Just (Form.FormSubmitted data) ->
                        ( { model | formData = updated }, sendFormData model data )

        CreateRecordResponse (Ok _) ->
            ( { model | formData = Form.init }, fetchRecordList )

        CreateRecordResponse (Err error) ->
            model |> updateError error

        EditRecord recordId ->
            ( model, fetchRecord recordId )

        EditRecordResponse (Ok _) ->
            ( model, fetchRecordList )

        EditRecordResponse (Err error) ->
            model |> updateError error

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordResponse (Ok record) ->
            ( { model
                | records = removeRecordFromList record model.records
                , error = Nothing
              }
            , fetchRecordList
            )

        DeleteRecordResponse (Err error) ->
            model |> updateError error


updateError : error -> Model -> ( Model, Cmd Msg )
updateError error model =
    ( { model | error = Just <| toString error }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- Helpers


recordToFormData : Record -> Form.Model
recordToFormData { id, title, description } =
    Form.Model
        (Just id)
        (Maybe.withDefault "" title)
        (Maybe.withDefault "" description)


encodeFormData : Form.Model -> Encode.Value
encodeFormData { title, description } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        ]


removeRecordFromList : Record -> List Record -> List Record
removeRecordFromList { id } records =
    List.filter (\record -> record.id /= id) records


updateRecordInList : Form.Model -> List Record -> List Record
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


updateRecord : Form.Model -> Record -> Record
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


decodeRecord : Decoder Record
decodeRecord =
    (map4 Record
        (field "id" string)
        (maybe (field "title" string))
        (maybe (field "description" string))
        (field "last_modified" int)
    )



-- Kinto API calls


fetchRecord : RecordId -> Cmd Msg
fetchRecord recordId =
    client
        |> Kinto.get recordResource recordId
        |> Kinto.send FetchRecordResponse


fetchRecordList : Cmd Msg
fetchRecordList =
    client
        |> Kinto.getList recordResource
        |> Kinto.sortBy [ "title", "description" ]
        |> Kinto.send FetchRecordsResponse


deleteRecord : RecordId -> Cmd Msg
deleteRecord recordId =
    client
        |> Kinto.delete recordResource recordId
        |> Kinto.send DeleteRecordResponse


sendFormData : Model -> Form.Model -> Cmd Msg
sendFormData model formData =
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
