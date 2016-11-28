module Kinto exposing (..)

import Base64
import Http
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
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


type alias Body =
    Encode.Value


type Request
    = KintoRequest Config Endpoint Method
    | KintoRequestWithBody Config Endpoint Method Body


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


fromRequest : Request -> Http.Request Encode.Value
fromRequest request =
    case request of
        KintoRequest config endpoint method ->
            Http.request
                { method = method
                , headers = headersFromConfig config
                , url = endpointUrl config endpoint
                , body = Http.emptyBody
                , expect = Http.expectJson bodyDataDecoder
                , timeout = Nothing
                , withCredentials = False
                }

        KintoRequestWithBody config endpoint method body ->
            Http.request
                { method = method
                , headers = headersFromConfig config
                , url = endpointUrl config endpoint
                , body = Http.jsonBody body
                , expect = Http.expectJson bodyDataDecoder
                , timeout = Nothing
                , withCredentials = False
                }


performQuery : Request -> (Result Error Decode.Value -> msg) -> Cmd msg
performQuery request toMsg =
    Http.send (toKintoResponse >> toMsg) (fromRequest request)


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


bodyDataDecoder : Decode.Decoder Decode.Value
bodyDataDecoder =
    (field "data" Decode.value)


errorDecoder : Decode.Decoder ErrorRecord
errorDecoder =
    Decode.map4 ErrorRecord
        (field "errno" Decode.int)
        (field "message" Decode.string)
        (field "code" Decode.int)
        (field "error" Decode.string)


toKintoResponse : Result Http.Error Decode.Value -> Result Error Decode.Value
toKintoResponse response =
    response
        |> Result.mapError extractError


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
    case Decode.decodeString errorDecoder body of
        Ok errRecord ->
            KintoError statusCode statusMsg errRecord

        Err err ->
            ServerError statusCode statusMsg err



-- High level API
-- GET


getBucketList : Config -> (Result Error Decode.Value -> msg) -> Cmd msg
getBucketList config toMsg =
    performQuery (KintoRequest config BucketListEndpoint "GET") toMsg


getBucket : Config -> BucketName -> (Result Error Decode.Value -> msg) -> Cmd msg
getBucket config bucket toMsg =
    performQuery (KintoRequest config (BucketEndpoint bucket) "GET") toMsg


getCollectionList :
    Config
    -> BucketName
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
getCollectionList config bucket toMsg =
    performQuery (KintoRequest config (CollectionListEndpoint bucket) "GET") toMsg


getCollection :
    Config
    -> BucketName
    -> CollectionName
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
getCollection config bucket collection toMsg =
    performQuery (KintoRequest config (CollectionEndpoint bucket collection) "GET") toMsg


getRecordList :
    Config
    -> BucketName
    -> CollectionName
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
getRecordList config bucket collection toMsg =
    performQuery (KintoRequest config (RecordListEndpoint bucket collection) "GET") toMsg


getRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
getRecord config bucket collection recordId toMsg =
    performQuery
        (KintoRequest
            config
            (RecordEndpoint bucket collection recordId)
            "GET"
        )
        toMsg



-- CREATE


createBucket : Config -> Body -> (Result Error Decode.Value -> msg) -> Cmd msg
createBucket config body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            BucketListEndpoint
            "POST"
            body
        )
        toMsg


createCollection :
    Config
    -> BucketName
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
createCollection config bucket body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (CollectionListEndpoint bucket)
            "POST"
            body
        )
        toMsg


createRecord :
    Config
    -> BucketName
    -> CollectionName
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
createRecord config bucket collection body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (RecordListEndpoint bucket collection)
            "POST"
            body
        )
        toMsg



-- UPDATE


updateBucket : Config -> BucketName -> Body -> (Result Error Decode.Value -> msg) -> Cmd msg
updateBucket config bucket body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (BucketEndpoint bucket)
            "PATCH"
            body
        )
        toMsg


updateCollection :
    Config
    -> BucketName
    -> CollectionName
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
updateCollection config bucket collection body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (CollectionEndpoint bucket collection)
            "PATCH"
            body
        )
        toMsg


updateRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
updateRecord config bucket collection recordId body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (RecordEndpoint bucket collection recordId)
            "PATCH"
            body
        )
        toMsg



-- REPLACE


replaceBucket : Config -> BucketName -> Body -> (Result Error Decode.Value -> msg) -> Cmd msg
replaceBucket config bucket body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (BucketEndpoint bucket)
            "PUT"
            body
        )
        toMsg


replaceCollection :
    Config
    -> BucketName
    -> CollectionName
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
replaceCollection config bucket collection body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (CollectionEndpoint bucket collection)
            "PUT"
            body
        )
        toMsg


replaceRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> Body
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
replaceRecord config bucket collection recordId body toMsg =
    performQuery
        (KintoRequestWithBody
            config
            (RecordEndpoint bucket collection recordId)
            "PUT"
            body
        )
        toMsg



-- DELETE


deleteBucket : Config -> BucketName -> (Result Error Decode.Value -> msg) -> Cmd msg
deleteBucket config bucket toMsg =
    performQuery
        (KintoRequest
            config
            (BucketEndpoint bucket)
            "DELETE"
        )
        toMsg


deleteCollection :
    Config
    -> BucketName
    -> CollectionName
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
deleteCollection config bucket collection toMsg =
    performQuery
        (KintoRequest
            config
            (CollectionEndpoint bucket collection)
            "DELETE"
        )
        toMsg


deleteRecord :
    Config
    -> BucketName
    -> CollectionName
    -> RecordId
    -> (Result Error Decode.Value -> msg)
    -> Cmd msg
deleteRecord config bucket collection recordId toMsg =
    performQuery
        (KintoRequest
            config
            (RecordEndpoint bucket collection recordId)
            "DELETE"
        )
        toMsg
