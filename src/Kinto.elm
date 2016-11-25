module Kinto exposing (..)

import Base64
import Http
import Json.Decode as Json exposing (field)
import String
import Task exposing (Task)


type alias Url =
    String


type alias Method =
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
    | BucketListEndpoint
    | BucketEndpoint BucketName
    | CollectionListEndpoint BucketName
    | CollectionEndpoint BucketName CollectionName
    | RecordListEndpoint BucketName CollectionName
    | RecordEndpoint BucketName CollectionName RecordId


type alias Config =
    { baseUrl : String
    , headers : List Http.Header
    , auth : Auth
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
    = ServerError StatusCode StatusMsg String
    | KintoError StatusCode StatusMsg ErrorRecord
    | NetworkError Http.Error



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

            BucketListEndpoint ->
                joinUrl [ baseUrl, "buckets" ]

            BucketEndpoint bucketName ->
                joinUrl [ baseUrl, "buckets", bucketName ]

            CollectionListEndpoint bucketName ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections" ]

            CollectionEndpoint bucketName collectionName ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName ]

            RecordListEndpoint bucketName collectionName ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName, "records" ]

            RecordEndpoint bucketName collectionName recordId ->
                joinUrl [ baseUrl, "buckets", bucketName, "collections", collectionName, "records", recordId ]


performQuery : Config -> Endpoint -> Method -> (Result Error Json.Value -> msg) -> Cmd msg
performQuery config endpoint method toMsg =
    let
        request =
            Http.request
                { method = method
                , headers = headersFromConfig config
                , url = endpointUrl config endpoint
                , body = Http.emptyBody
                , expect = Http.expectJson bodyDataDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (toKintoResponse toMsg) request


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    { config | headers = (Http.header name value) :: headers }


alwaysEncode : String -> String
alwaysEncode string =
    case Base64.encode string of
        Err msg ->
            Debug.crash "b64encoding failed" msg

        Ok hash ->
            hash


headersFromConfig : Config -> List Http.Header
headersFromConfig ({ auth, headers } as config) =
    case auth of
        NoAuth ->
            headers

        Basic username password ->
            config
                |> withHeader
                    "Authorization"
                    ("Basic " ++ (alwaysEncode (username ++ ":" ++ password)))
                |> .headers

        Bearer token ->
            config
                |> withHeader "Authorization" ("Bearer " ++ token)
                |> .headers


configure : Url -> Auth -> Config
configure baseUrl auth =
    Config baseUrl [] auth



-- Dealing with answers from the Kinto server


bodyDataDecoder : Json.Decoder Json.Value
bodyDataDecoder =
    (field "data" Json.value)


errorDecoder : Json.Decoder ErrorRecord
errorDecoder =
    Json.map4 ErrorRecord
        (field "errno" Json.int)
        (field "message" Json.string)
        (field "code" Json.int)
        (field "error" Json.string)


toKintoResponse :
    (Result Error Json.Value -> msg)
    -> Result Http.Error Json.Value
    -> msg
toKintoResponse toMsg response =
    response
        |> Result.mapError extractError
        |> toMsg


extractError : Http.Error -> Error
extractError error =
    case error of
        Http.BadStatus { status, body } ->
            extractKintoError status.code status.message body

        Http.BadPayload str { status, body } ->
            ServerError
                status.code
                status.message
                ("failed decoding json: "
                    ++ str
                    ++ "\n\nBody received from server: "
                    ++ body
                )

        anyError ->
            NetworkError anyError


extractKintoError : StatusCode -> StatusMsg -> String -> Error
extractKintoError statusCode statusMsg body =
    case Json.decodeString errorDecoder body of
        Ok errRecord ->
            KintoError statusCode statusMsg errRecord

        Err err ->
            ServerError statusCode statusMsg err



-- High level API
-- GET


getBucketList : Config -> (Result Error Json.Value -> msg) -> Cmd msg
getBucketList config =
    performQuery config BucketListEndpoint "GET"


getBucket : Config -> BucketName -> (Result Error Json.Value -> msg) -> Cmd msg
getBucket config bucket =
    performQuery config (BucketEndpoint bucket) "GET"


getCollectionList :
    Config
    -> BucketName
    -> (Result Error Json.Value -> msg)
    -> Cmd msg
getCollectionList config bucket =
    performQuery config (CollectionListEndpoint bucket) "GET"


getCollection :
    Config
    -> BucketName
    -> CollectionName
    -> (Result Error Json.Value -> msg)
    -> Cmd msg
getCollection config bucket collection =
    performQuery config (CollectionEndpoint bucket collection) "GET"


getRecordList :
    Config
    -> BucketName
    -> CollectionName
    -> (Result Error Json.Value -> msg)
    -> Cmd msg
getRecordList config bucket collection toMsg =
    performQuery config (RecordListEndpoint bucket collection) "GET" toMsg


getRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> (Result Error Json.Value -> msg)
    -> Cmd msg
getRecord config bucket collection recordId toMsg =
    performQuery
        config
        (RecordEndpoint bucket collection recordId)
        "GET"
        toMsg
