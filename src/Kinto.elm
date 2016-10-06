module Kinto exposing (..)

import Base64
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (Task)


type alias Url =
    String


type alias Verb =
    String



-- Auth


type alias Username =
    String


type alias Password =
    String


type alias Token =
    String


type Auth
    = NoAuth
    | Basic Username Password
    | Bearer Token



-- Kinto types


type alias BucketName =
    String


type alias CollectionName =
    String


type alias RecordId =
    String


type Endpoint
    = RootEndpoint
    | BucketEndpoint (Maybe BucketName)
    | CollectionEndpoint BucketName (Maybe CollectionName)
    | RecordEndpoint BucketName CollectionName (Maybe RecordId)


type alias Config =
    { baseUrl : String
    , headers : List ( String, String )
    }



-- Kinto errors


type alias ErrorRecord =
    { errno : Int
    , message : String
    , code : Int
    , error : String
    }


type alias StatusCode =
    Int


type alias StatusMsg =
    String


type Error
    = ConfigError String
    | ServerError StatusCode StatusMsg String
    | KintoError StatusCode StatusMsg ErrorRecord
    | NetworkError Http.RawError



-- Making requests


endpointUrl : Config -> Endpoint -> Url
endpointUrl config endpoint =
    let
        baseUrl =
            if String.endsWith "/" config.baseUrl then
                String.dropRight 1 config.baseUrl
            else
                config.baseUrl

        joinUrl =
            String.join "/"
    in
        case endpoint of
            RootEndpoint ->
                -- Otherwise Kinto returns a 307
                -- See https://github.com/Kinto/kinto/issues/852
                baseUrl ++ "/"

            BucketEndpoint Nothing ->
                joinUrl [ baseUrl, "buckets" ]

            BucketEndpoint (Just bucketName) ->
                joinUrl [ baseUrl, "buckets", bucketName ]

            CollectionEndpoint bucketName Nothing ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections" ]

            CollectionEndpoint bucketName (Just collectionName) ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName ]

            RecordEndpoint bucketName collectionName Nothing ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName, "records" ]

            RecordEndpoint bucketName collectionName (Just recordId) ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName, "records", recordId ]


client : Config -> Endpoint -> Verb -> Task Error Json.Value
client config endpoint verb =
    let
        request =
            { verb = verb
            , url = endpointUrl config endpoint
            , headers = config.headers
            , body = Http.empty
            }
    in
        (Http.send Http.defaultSettings request)
            |> toKintoResponse


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    let
        newHeaders =
            ( name, value ) :: headers
    in
        { config | headers = newHeaders }


withAuthHeader : Auth -> Config -> Result Error Config
withAuthHeader auth config =
    case auth of
        NoAuth ->
            Ok config

        Basic username password ->
            let
                hash str =
                    Base64.encode str |> Result.formatError ConfigError

                addAuthHeader hash =
                    Ok (withHeader "Authorization" ("Basic " ++ hash) config)
            in
                hash (username ++ ":" ++ password)
                    `Result.andThen` addAuthHeader

        Bearer token ->
            Ok (config |> withHeader "Authorization" ("Bearer " ++ token))


configure : Url -> Auth -> Result Error Config
configure baseUrl auth =
    Config baseUrl []
        |> withAuthHeader auth
        |> Debug.log "Kinto request"



-- Dealing with answers from the Kinto server


bodyDataDecoder : Json.Decoder Json.Value
bodyDataDecoder =
    ("data" := Json.value)


extractData : String -> StatusCode -> StatusMsg -> Task Error Json.Value
extractData body statusCode statusMsg =
    let
        responseResult =
            Json.decodeString bodyDataDecoder body
    in
        case responseResult of
            Ok data ->
                Task.succeed data

            Err _ ->
                -- Is it an ErrorRecord json?
                decodeError body statusCode statusMsg


errorDecoder : Json.Decoder ErrorRecord
errorDecoder =
    Json.object4 ErrorRecord
        ("errno" := Json.int)
        ("message" := Json.string)
        ("code" := Json.int)
        ("error" := Json.string)


decodeError : String -> StatusCode -> StatusMsg -> Task Error a
decodeError body statusCode statusMsg =
    case Json.decodeString errorDecoder body of
        Ok errorRecord ->
            Task.fail (KintoError statusCode statusMsg errorRecord)

        Err msg ->
            Task.fail (ServerError statusCode statusMsg body)


toKintoResponse : Task Http.RawError Http.Response -> Task Error Json.Value
toKintoResponse response =
    (Task.mapError NetworkError response)
        `Task.andThen` (handleResponse extractData)


handleResponse :
    (String -> StatusCode -> StatusMsg -> Task Error Json.Value)
    -> Http.Response
    -> Task Error Json.Value
handleResponse handle response =
    case response.value of
        Http.Text body ->
            handle body response.status response.statusText

        _ ->
            Task.fail
                (ServerError
                    response.status
                    response.statusText
                    "Response body is a blob"
                )



-- High level API


performQuery : Result Error Config -> Endpoint -> Verb -> Task Error Json.Value
performQuery config endpoint verb =
    case config of
        Err error ->
            Task.fail error

        Ok config ->
            client config endpoint verb



-- GET


getBucketList : Result Error Config -> Task Error Json.Value
getBucketList config =
    performQuery config (BucketEndpoint Nothing) "GET"


getBucket : Result Error Config -> BucketName -> Task Error Json.Value
getBucket config bucket =
    performQuery config (BucketEndpoint (Just bucket)) "GET"


getCollectionList : Result Error Config -> BucketName -> Task Error Json.Value
getCollectionList config bucket =
    performQuery config (CollectionEndpoint bucket Nothing) "GET"


getCollection :
    Result Error Config
    -> BucketName
    -> CollectionName
    -> Task Error Json.Value
getCollection config bucket collection =
    performQuery config (CollectionEndpoint bucket (Just collection)) "GET"


getRecordList :
    Result Error Config
    -> BucketName
    -> CollectionName
    -> Task Error Json.Value
getRecordList config bucket collection =
    performQuery config (RecordEndpoint bucket collection Nothing) "GET"


getRecord :
    Result Error Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> Task Error Json.Value
getRecord config bucket collection recordId =
    performQuery
        config
        (RecordEndpoint bucket collection (Just recordId))
        "GET"
