module Model exposing (..)

import Task
import Time exposing (Time, second)
import HttpBuilder exposing (withJsonBody, withHeader, withExpect)
import Json.Decode exposing (Decoder, string, at, list, map4, field, maybe, int, Value, decodeValue)
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
    | FetchRecordResponse (Result Kinto.Error Value)
    | FetchRecords
    | FetchRecordsResponse (Result Kinto.Error Value)
    | FormMsg Form.Msg
    | CreateRecordResponse (Result Http.Error Record)
    | EditRecord RecordId
    | EditRecordResponse (Result Http.Error Record)
    | DeleteRecord RecordId
    | DeleteRecordResponse (Result Http.Error Record)


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch [ fetchRecords initialModel ]
    )


config =
    Kinto.configure
        "https://kinto.dev.mozaws.net/v1/"
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
            ( { model | records = Dict.empty, error = Nothing }, fetchRecords model )

        FetchRecordResponse response ->
            case response of
                Ok data ->
                    case (decodeValue decodeRecord data) of
                        Err msg ->
                            ( { model | error = Just msg }, Cmd.none )

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
                Ok data ->
                    let
                        recordsToDict records =
                            List.map (\r -> ( r.id, r )) records
                                |> Dict.fromList
                    in
                        case (decodeValue decodeRecords data) of
                            Err msg ->
                                ( { model | error = Just msg }, Cmd.none )

                            Ok recordList ->
                                ( { model
                                    | records = recordsToDict recordList
                                    , error = Nothing
                                  }
                                , Cmd.none
                                )

                Err error ->
                    let
                        _ =
                            Debug.log "FetchRecordsResponse failed:" error
                    in
                        ( model, Cmd.none )

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

        CreateRecordResponse response ->
            case response of
                Ok _ ->
                    ( { model | formData = Form.init }, fetchRecords model )

                Err err ->
                    ( { model | error = Just (toString err) }, Cmd.none )

        EditRecord recordId ->
            ( model, fetchRecord model recordId )

        EditRecordResponse response ->
            case response of
                Ok _ ->
                    ( model, fetchRecords model )

                Err err ->
                    ( { model | error = Just (toString err) }, Cmd.none )

        DeleteRecord recordId ->
            ( model, deleteRecord recordId )

        DeleteRecordResponse response ->
            case response of
                Ok data ->
                    ( { model
                        | records = removeRecordFromList data model.records
                        , error = Nothing
                      }
                    , fetchRecords model
                    )

                Err err ->
                    ( { model | error = Just (toString err) }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- HTTP


fetchRecord : Model -> RecordId -> Cmd Msg
fetchRecord model recordId =
    Kinto.getRecord
        model.kintoConfig
        "default"
        "test-items"
        recordId
        FetchRecordResponse


fetchRecords : Model -> Cmd Msg
fetchRecords model =
    Kinto.getRecordList
        model.kintoConfig
        "default"
        "test-items"
        FetchRecordsResponse


decodeRecords : Decoder (List Record)
decodeRecords =
    list decodeRecord


decodeRecord : Decoder Record
decodeRecord =
    map4 Record
        (field "id" string)
        (maybe (field "title" string))
        (maybe (field "description" string))
        (field "last_modified" int)


sendFormData : Form.Model -> Cmd Msg
sendFormData formData =
    -- TODO: handle auth with provided credentials
    let
        rootUrl =
            "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"

        ( method, url, responseMsg ) =
            case formData.id of
                Nothing ->
                    ( HttpBuilder.post, rootUrl, CreateRecordResponse )

                Just id ->
                    ( HttpBuilder.patch, rootUrl ++ "/" ++ id, EditRecordResponse )

        request =
            method url
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> withJsonBody (encodeFormData formData)
                |> withExpect (Http.expectJson (field "data" decodeRecord))
    in
        HttpBuilder.send responseMsg request


deleteRecord : RecordId -> Cmd Msg
deleteRecord recordId =
    -- TODO: handle auth with provided credentials
    let
        delete_url =
            "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records/" ++ recordId

        request =
            HttpBuilder.delete delete_url
                |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
                |> withExpect (Http.expectJson (field "data" decodeRecord))
    in
        HttpBuilder.send DeleteRecordResponse request


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
