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


type alias Headers =
    List ( String, String )



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
    , headers : Headers
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


performQuery : Config -> Endpoint -> Verb -> Task Error Json.Value
performQuery config endpoint verb =
    let
        request =
            { verb = verb
            , url = endpointUrl config endpoint
            , headers = headersFromConfig config
            , body = Http.empty
            }
    in
        ((Http.send Http.defaultSettings request)
            |> Task.mapError NetworkError
        )
            `Task.andThen` (toKintoResponse >> Task.fromResult)


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    { config | headers = ( name, value ) :: headers }


alwaysEncode : String -> String
alwaysEncode string =
    case Base64.encode string of
        Err msg ->
            Debug.crash "b64encoding failed" msg

        Ok hash ->
            hash


headersFromConfig : Config -> Headers
headersFromConfig ({ auth, headers } as config) =
    case auth of
        NoAuth ->
            headers

        Basic username password ->
            config
                |> withHeader "Authorization"
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
    ("data" := Json.value)


errorDecoder : Json.Decoder ErrorRecord
errorDecoder =
    Json.object4 ErrorRecord
        ("errno" := Json.int)
        ("message" := Json.string)
        ("code" := Json.int)
        ("error" := Json.string)


toKintoResponse : Http.Response -> Result Error Json.Value
toKintoResponse ({ value, status, statusText } as response) =
    (extractBody response)
        `Result.andThen`
            (extractData
                >> Result.formatError (extractError status statusText)
            )


extractBody : Http.Response -> Result Error String
extractBody { value, status, statusText } =
    case value of
        Http.Text body ->
            Ok body

        _ ->
            Err (ServerError status statusText "Unsupported response body")


extractData : String -> Result String Json.Value
extractData data =
    Json.decodeString bodyDataDecoder data


extractError : StatusCode -> StatusMsg -> String -> Error
extractError statusCode statusMsg body =
    case Json.decodeString errorDecoder body of
        Ok errRecord ->
            KintoError statusCode statusMsg errRecord

        Err err ->
            ServerError statusCode statusMsg err



-- High level API
-- GET


getBucketList : Config -> Task Error Json.Value
getBucketList config =
    performQuery config BucketListEndpoint "GET"


getBucket : Config -> BucketName -> Task Error Json.Value
getBucket config bucket =
    performQuery config (BucketEndpoint bucket) "GET"


getCollectionList : Config -> BucketName -> Task Error Json.Value
getCollectionList config bucket =
    performQuery config (CollectionListEndpoint bucket) "GET"


getCollection : Config -> BucketName -> CollectionName -> Task Error Json.Value
getCollection config bucket collection =
    performQuery config (CollectionEndpoint bucket collection) "GET"


getRecordList : Config -> BucketName -> CollectionName -> Task Error Json.Value
getRecordList config bucket collection =
    performQuery config (RecordListEndpoint bucket collection) "GET"


getRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> Task Error Json.Value
getRecord config bucket collection recordId =
    performQuery
        config
        (RecordEndpoint bucket collection recordId)
        "GET"
