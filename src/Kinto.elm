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
    | BucketListEndpoint
    | BucketEndpoint BucketName
    | CollectionListEndpoint BucketName
    | CollectionEndpoint BucketName CollectionName
    | RecordListEndpoint BucketName CollectionName
    | RecordEndpoint BucketName CollectionName RecordId


type alias Config =
    { baseUrl : String
    , headers : List ( String, String )
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
        (Http.send Http.defaultSettings request)
            |> toKintoResponse


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    let
        newHeaders =
            ( name, value ) :: headers
    in
        { config | headers = newHeaders }


headersFromConfig : Config -> List ( String, String )
headersFromConfig config =
    case config.auth of
        NoAuth ->
            config.headers

        Basic username password ->
            let
                hash =
                    case Base64.encode (username ++ ":" ++ password) of
                        Err msg ->
                            Debug.crash "b64encoding failed" msg

                        Ok hash ->
                            hash
            in
                config
                    |> withHeader "Authorization" ("Basic " ++ hash)
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
                Task.fail (decodeError body statusCode statusMsg)


errorDecoder : Json.Decoder ErrorRecord
errorDecoder =
    Json.object4 ErrorRecord
        ("errno" := Json.int)
        ("message" := Json.string)
        ("code" := Json.int)
        ("error" := Json.string)


decodeError : String -> StatusCode -> StatusMsg -> Error
decodeError body statusCode statusMsg =
    case Json.decodeString errorDecoder body of
        Ok errorRecord ->
            KintoError statusCode statusMsg errorRecord

        Err msg ->
            ServerError statusCode statusMsg body


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
