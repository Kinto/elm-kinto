module Model exposing (..)

import Task
import Time exposing (Time, second)
import HttpBuilder exposing (Response, Error, send, withJsonBody, withHeader, jsonReader, stringReader)
import Json.Decode exposing (Decoder, string, at, list, object4, (:=), maybe, int)
import Json.Encode as Encode
import Form
import List


-- TODO:
-- - Expose only what's necessary


type alias RecordId =
    String


type alias Record =
    { id : RecordId
    , title : Maybe String
    , description : Maybe String
    , last_modified : Int
    }


type alias FormData =
    { title : String
    , description : String
    }


type alias Model =
    { error : Bool
    , errorMsg : String
    , records : List Record
    , formData : Form.Model
    , currentTime : Time
    }


type Msg
    = NoOp
    | Tick Time
    | FetchRecordSucceed (Response Record)
    | FetchRecordFail (Error String)
    | FetchRecords
    | FetchRecordsSucceed (Response (List Record))
    | FetchRecordsFail (Error String)
    | FormMsg Form.Msg
    | CreateSucceed (Response Record)
    | CreateFail (Error String)
    | EditRecord RecordId
    | EditRecordSucceed (Response Record)
    | EditRecordFail (Error String)
    | DeleteRecord RecordId
    | DeleteRecordSucceed (Response Record)
    | DeleteRecordFail (Error String)


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , fetchRecords
    )


initialModel : Model
initialModel =
    { error = False
    , errorMsg = ""
    , records = []
    , formData = Form.init
    , currentTime = 0
    }


recordToFormData : Record -> Form.Model
recordToFormData { title, description } =
    Form.Model
        (Maybe.withDefault "" title)
        (Maybe.withDefault "" description)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        FetchRecords ->
            ( { model | records = [], error = False }, fetchRecords )

        FetchRecordSucceed { data } ->
            -- XXX: map Record -> FormData
            ( { model | formData = recordToFormData data, error = False }, Cmd.none )

        FetchRecordFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )

        FetchRecordsSucceed { data } ->
            ( { model | records = data, error = False }, Cmd.none )

        FetchRecordsFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )

        FormMsg subMsg ->
            let
                ( updated, formMsg ) =
                    Form.update subMsg model.formData
            in
                case formMsg of
                    Nothing ->
                        ( { model | formData = updated }, Cmd.none )

                    Just (Form.FormSubmitted data) ->
                        ( { model | formData = updated }
                        , createRecord data
                        )

        CreateSucceed _ ->
            ( { model | formData = Form.init }, fetchRecords )

        CreateFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )

        EditRecord recordId ->
            ( model, fetchRecord recordId )

        EditRecordSucceed { data } ->
            ( model, Cmd.none )

        EditRecordFail err ->
            ( model, Cmd.none )

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordSucceed { data } ->
            ( { model
                | records = removeRecordFromList data model.records
                , error = False
              }
            , Cmd.none
            )

        DeleteRecordFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- HTTP


fetchRecord : RecordId -> Cmd Msg
fetchRecord recordId =
    -- TODO: handle auth with provided credentials
    let
        request =
            HttpBuilder.get ("https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records/" ++ recordId)
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> send (jsonReader (at [ "data" ] decodeRecord)) stringReader
    in
        Task.perform FetchRecordFail FetchRecordSucceed request


fetchRecords : Cmd Msg
fetchRecords =
    -- TODO: handle auth with provided credentials
    let
        request =
            HttpBuilder.get "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> send (jsonReader decodeRecords) stringReader
    in
        Task.perform FetchRecordsFail FetchRecordsSucceed request


decodeRecords : Decoder (List Record)
decodeRecords =
    at [ "data" ] (list decodeRecord)


decodeRecord : Decoder Record
decodeRecord =
    object4 Record
        ("id" := string)
        (maybe ("title" := string))
        (maybe ("description" := string))
        ("last_modified" := int)


createRecord : FormData -> Cmd Msg
createRecord formData =
    -- TODO: handle auth with provided credentials
    let
        request =
            HttpBuilder.post "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"
                |> withHeader "Content-Type" "application/json"
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> withJsonBody (encodeFormData formData)
                |> send (jsonReader (at [ "data" ] decodeRecord)) stringReader
    in
        Task.perform CreateFail CreateSucceed request


deleteRecord : RecordId -> Cmd Msg
deleteRecord recordId =
    -- TODO: handle auth with provided credentials
    let
        delete_url =
            "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records/" ++ recordId

        request =
            HttpBuilder.delete delete_url
                |> withHeader "Content-Type" "application/json"
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> send (jsonReader (at [ "data" ] decodeRecord)) stringReader
    in
        Task.perform DeleteRecordFail DeleteRecordSucceed request


encodeFormData : FormData -> Encode.Value
encodeFormData { title, description } =
    Encode.object
        [ ( "data"
          , Encode.object
                [ ( "title", Encode.string title )
                , ( "description", Encode.string description )
                ]
          )
        ]


removeRecordFromList : Record -> List Record -> List Record
removeRecordFromList { id } records =
    List.filter (\record -> record.id /= id) records
