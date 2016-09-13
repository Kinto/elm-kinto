module Model exposing (..)

import Task
import HttpBuilder exposing (Response, Error, send, withJsonBody, withHeader, jsonReader, stringReader)
import Json.Decode exposing (Decoder, string, at, list, object3, (:=), maybe)
import Json.Encode as Encode

import Form


-- TODO:
-- - Expose only what's necessary


type alias RecordId =
    String


type alias Record =
    -- XXX add last_modified
    { id : RecordId
    , title : Maybe String
    , description : Maybe String
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
    }


type Msg
    = NoOp
    | FetchRecords
    | FetchRecordsSucceed (Response (List Record))
    | FetchRecordsFail (Error String)
    | FormMsg Form.Msg
    | SubmitForm
    | CreateSucceed (Response Record)
    | CreateFail (Error String)


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
    }


initialFormData : FormData
initialFormData =
    { title = ""
    , description = ""
    }


updateFormDataTitle : FormData -> String -> FormData
updateFormDataTitle formData title =
    -- no really there should be a better way
    { formData | title = title }


updateFormDataDescription : FormData -> String -> FormData
updateFormDataDescription formData description =
    -- no really there should be a better way
    { formData | description = description }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchRecords ->
            ( { model | records = [], error = False }, fetchRecords )

        FetchRecordsSucceed { data } ->
            ( { model | records = data, error = False }, Cmd.none )

        FetchRecordsFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )

        FormMsg subMsg ->
            ( { model | formData = Form.update subMsg model.formData }, Cmd.none )

        SubmitForm ->
            ( model, createRecord model.formData )

        CreateSucceed _ ->
            ( { model | formData = initialFormData }, fetchRecords )

        CreateFail err ->
            ( { model | error = True, errorMsg = (toString err) }, Cmd.none )



-- HTTP


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
    object3 Record
        ("id" := string)
        (maybe ("title" := string))
        (maybe ("description" := string))


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
