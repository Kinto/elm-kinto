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
    { id : Maybe String
    , title : String
    , description : String
    }


type alias Model =
    { error : Maybe String
    , records : List Record
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
    | CreateSucceed (Response Record)
    | EditRecord RecordId
    | EditRecordSucceed (Response Record)
    | DeleteRecord RecordId
    | DeleteRecordSucceed (Response Record)


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , fetchRecords
    )


initialModel : Model
initialModel =
    { error = Nothing
    , records = []
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
            ( { model | records = [], error = Nothing }, fetchRecords )

        FetchRecordSucceed { data } ->
            ( { model | formData = recordToFormData data, error = Nothing }, Cmd.none )

        FetchRecordsSucceed { data } ->
            ( { model | records = data, error = Nothing }, Cmd.none )

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

        CreateSucceed _ ->
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
            , Cmd.none
            )



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


sendFormData : FormData -> Cmd Msg
sendFormData formData =
    -- TODO: handle auth with provided credentials
    let
        rootUrl =
            "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"

        ( method, url, failureMsg, successMsg ) =
            case formData.id of
                Nothing ->
                    ( HttpBuilder.post, rootUrl, HttpFail, CreateSucceed )

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


updateRecordInList : Form.Model -> List Record -> List Record
updateRecordInList { id, title, description } records =
    -- This enables live reflecting ongoing form updates in the records list
    case id of
        Nothing ->
            records

        Just _ ->
            List.map
                (\record ->
                    if record.id == (Maybe.withDefault "" id) then
                        { record
                            | title = Just title
                            , description = Just description
                        }
                    else
                        record
                )
                records
