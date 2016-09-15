module Model exposing (..)

import Task
import HttpBuilder exposing (Response, Error, send, withJsonBody, withHeader, jsonReader, stringReader)
import Json.Decode exposing (Decoder, string, at, list, object4, (:=), maybe, int)
import Json.Encode as Encode
import Form


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
    }


type Msg
    = NoOp
    | FetchRecords
    | FetchRecordsSucceed (Response (List Record))
    | FetchRecordsFail (Error String)
    | FormMsg Form.Msg
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
            let
                ( updated, formMsg ) =
                    Form.update subMsg model.formData
            in
                case formMsg of
                    Nothing ->
                        ( { model | formData = updated }, Cmd.none )

                    Just (Form.FormSubmitted data) ->
                        ( { model | formData = updated }
                        , createRecord data )

        CreateSucceed _ ->
            ( { model | formData = Form.init }, fetchRecords )

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
