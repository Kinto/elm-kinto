module Model exposing (..)

import Task
import Time exposing (Time, second)
import HttpBuilder exposing (withJsonBody, withHeader, withExpect)
import Json.Decode as Decode exposing (Decoder, string, at, list, map4, field, maybe, int, Value, decodeValue)
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
    , kintoConfig : Kinto.Config
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
    , Cmd.batch [ fetchRecords ]
    )


config =
    Kinto.configure
        "https://kinto.dev.mozaws.net/v1/"
        (Kinto.Basic "test" "test")


recordConfig recordId =
    Kinto.configureResource
        "https://kinto.dev.mozaws.net/v1/"
        (Kinto.RecordEndpoint "default" "test-items" recordId)
        (Kinto.Basic "test" "test")


recordListConfig =
    Kinto.configureResource
        "https://kinto.dev.mozaws.net/v1/"
        (Kinto.RecordListEndpoint "default" "test-items")
        (Kinto.Basic "test" "test")


initialModel : Model
initialModel =
    { error = Nothing
    , records = Dict.empty
    , formData = Form.init
    , currentTime = 0
    , kintoConfig = config
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

        FetchRecords ->
            ( { model | records = Dict.empty, error = Nothing }, fetchRecords )

        FetchRecordResponse response ->
            case response of
                Ok record ->
                    ( { model
                        | formData = recordToFormData record
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just <| toString error }, Cmd.none )

        FetchRecordsResponse response ->
            case response of
                Ok recordList ->
                    let
                        recordsToDict records =
                            List.map (\r -> ( r.id, r )) records
                                |> Dict.fromList
                    in
                        ( { model
                            | records = recordsToDict recordList
                            , error = Nothing
                          }
                        , Cmd.none
                        )

                Err error ->
                    ( { model | error = Just <| toString error }, Cmd.none )

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

        CreateRecordResponse response ->
            case response of
                Ok _ ->
                    ( { model | formData = Form.init }, fetchRecords )

                Err error ->
                    ( { model | error = Just <| toString error }, Cmd.none )

        EditRecord recordId ->
            ( model, fetchRecord recordId )

        EditRecordResponse response ->
            case response of
                Ok _ ->
                    ( model, fetchRecords )

                Err err ->
                    ( { model | error = Just <| toString err }, Cmd.none )

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordResponse response ->
            case response of
                Ok record ->
                    ( { model
                        | records = removeRecordFromList record model.records
                        , error = Nothing
                      }
                    , fetchRecords
                    )

                Err err ->
                    ( { model | error = Just <| toString err }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- HTTP


fetchRecord : RecordId -> Cmd Msg
fetchRecord recordId =
    recordConfig recordId
        |> Kinto.get
        |> Kinto.withDecoder (decodeData decodeRecord)
        |> Kinto.send FetchRecordResponse


fetchRecords : Cmd Msg
fetchRecords =
    recordListConfig
        |> Kinto.get
        |> Kinto.withDecoder (decodeData decodeRecords)
        |> Kinto.send FetchRecordsResponse


deleteRecord : RecordId -> Cmd Msg
deleteRecord recordId =
    recordConfig recordId
        |> Kinto.delete
        |> Kinto.withDecoder (decodeData decodeRecord)
        |> Kinto.send DeleteRecordResponse


decodeData : Decoder a -> Decoder a
decodeData decoder =
    field "data" decoder


decodeRecords : Decoder (List Record)
decodeRecords =
    list decodeRecord


decodeRecord : Decoder Record
decodeRecord =
    (map4 Record
        (field "id" string)
        (maybe (field "title" string))
        (maybe (field "description" string))
        (field "last_modified" int)
    )


sendFormData : Model -> Form.Model -> Cmd Msg
sendFormData model formData =
    let
        data =
            encodeFormData formData
    in
        case formData.id of
            Nothing ->
                recordListConfig
                    |> Kinto.create data
                    |> Kinto.withDecoder (decodeData decodeRecord)
                    |> Kinto.send CreateRecordResponse

            Just recordId ->
                recordConfig recordId
                    |> Kinto.update data
                    |> Kinto.withDecoder (decodeData decodeRecord)
                    |> Kinto.send EditRecordResponse


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
