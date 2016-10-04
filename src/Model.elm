module Model exposing (..)

import Task
import Time exposing (Time, second)
import HttpBuilder exposing (Response, Error, send, withJsonBody, withHeader, jsonReader, stringReader)
import Json.Decode exposing (Decoder, string, at, list, object4, (:=), maybe, int)
import Json.Encode as Encode
import Form
import Dict
import Http
import Kinto


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


type alias Records =
    Dict.Dict RecordId Record


type alias Model =
    { error : Maybe String
    , records : Records
    , formData : Form.Model
    , currentTime : Time
    }


type Msg
    = NoOp
    | Tick Time
    | HttpFail (Error String)
    | FetchRecordSucceed (Response Record)
    | FetchRecords
    | FetchRecordsSucceed (Response (List Record))
    | FormMsg Form.Msg
    | CreateRecordSucceed (Response Record)
    | EditRecord RecordId
    | EditRecordSucceed (Response Record)
    | DeleteRecord RecordId
    | DeleteRecordSucceed (Response Record)
    | TestClientFail Http.RawError
    | TestClient Http.Response


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch [ fetchRecords, testClient ]
    )



-- client : Config -> Endpoint -> Verb -> Task Http.RawError Http.Response


testClient : Cmd Msg
testClient =
    let
        config =
            Kinto.Config "https://kinto.dev.mozaws.net/v1/" []

        client =
            Kinto.client config Kinto.RootEndpoint "GET"
    in
        -- Kinto.request HttpError KintoError KintoSucces client
        Task.perform TestClientFail TestClient client


initialModel : Model
initialModel =
    { error = Nothing
    , records = Dict.empty
    , formData = Form.init
    , currentTime = 0
    }


recordToFormData : Record -> Form.Model
recordToFormData { id, title, description } =
    Form.Model
        (Just id)
        (Maybe.withDefault "" title)
        (Maybe.withDefault "" description)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        HttpFail err ->
            ( { model | error = Just (toString err) }, Cmd.none )

        FetchRecords ->
            ( { model | records = Dict.empty, error = Nothing }, fetchRecords )

        FetchRecordSucceed { data } ->
            ( { model | formData = recordToFormData data, error = Nothing }, Cmd.none )

        FetchRecordsSucceed { data } ->
            ( { model
                | records =
                    List.map (\r -> ( r.id, r )) data
                        |> Dict.fromList
                , error = Nothing
              }
            , Cmd.none
            )

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
                        ( { model | formData = updated }, sendFormData data )

        CreateRecordSucceed _ ->
            ( { model | formData = Form.init }, fetchRecords )

        EditRecord recordId ->
            ( model, fetchRecord recordId )

        EditRecordSucceed { data } ->
            ( model, fetchRecords )

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordSucceed { data } ->
            ( { model
                | records = removeRecordFromList data model.records
                , error = Nothing
              }
            , fetchRecords
            )

        TestClient response ->
            let
                _ =
                    Debug.log "response" response
            in
                ( model, Cmd.none )

        TestClientFail err ->
            let
                _ =
                    Debug.log "error" err
            in
                ( model, Cmd.none )



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
        Task.perform HttpFail FetchRecordSucceed request


fetchRecords : Cmd Msg
fetchRecords =
    -- TODO: handle auth with provided credentials
    let
        request =
            HttpBuilder.get "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> send (jsonReader decodeRecords) stringReader
    in
        Task.perform HttpFail FetchRecordsSucceed request


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


sendFormData : Form.Model -> Cmd Msg
sendFormData formData =
    -- TODO: handle auth with provided credentials
    let
        rootUrl =
            "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"

        ( method, url, failureMsg, successMsg ) =
            case formData.id of
                Nothing ->
                    ( HttpBuilder.post, rootUrl, HttpFail, CreateRecordSucceed )

                Just id ->
                    ( HttpBuilder.patch, rootUrl ++ "/" ++ id, HttpFail, EditRecordSucceed )

        request =
            method url
                |> withHeader "Content-Type" "application/json"
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> withJsonBody (encodeFormData formData)
                |> send (jsonReader (at [ "data" ] decodeRecord)) stringReader
    in
        Task.perform failureMsg successMsg request


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
        Task.perform HttpFail DeleteRecordSucceed request


encodeFormData : Form.Model -> Encode.Value
encodeFormData { title, description } =
    Encode.object
        [ ( "data"
          , Encode.object
                [ ( "title", Encode.string title )
                , ( "description", Encode.string description )
                ]
          )
        ]


removeRecordFromList : Record -> Records -> Records
removeRecordFromList { id } records =
    Dict.remove id records


updateRecordInList : Form.Model -> Records -> Records
updateRecordInList formData records =
    -- This enables live reflecting ongoing form updates in the records list
    case formData.id of
        Nothing ->
            records

        Just id ->
            Dict.update id (updateRecord formData) records


updateRecord : Form.Model -> Maybe Record -> Maybe Record
updateRecord formData record =
    case record of
        Nothing ->
            record

        Just record ->
            Just
                { record
                    | title = Just formData.title
                    , description = Just formData.description
                }
