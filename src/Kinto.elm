module Kinto exposing (..)

import Http
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
    = Basic Username Password
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



-- Kinto implementation


endpointUrl : Config -> Endpoint -> Url
endpointUrl config endpoint =
    let
        baseUrl =
            if String.endsWith "/" config.baseUrl then
                String.dropRight 1 config.baseUrl
            else
                config.baseUrl
        joinUrl = String.join "/"
    in
        case endpoint of
            RootEndpoint ->
                baseUrl

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

makeRequest : Config -> Endpoint -> Verb -> Http.Request
makeRequest config endpoint verb =
    { verb = verb
    , url = endpointUrl config endpoint
    , headers = config.headers
    , body = Http.empty
    }


client : Config -> Endpoint -> Verb -> Task Http.RawError Http.Response
client config endpoint verb =
    let
        request = makeRequest config endpoint verb
    in
        Http.send Http.defaultSettings request



-- -- client myConfig RootEndpoint Get
-- client
--     |> getRootEndpoint
--     |> Result.formatError toString  -- in case of an error
--     |> Html.text  -- if result is ok
--     |> createBucket "BucketName" {}
--     |> getBucketList
--     |> putBucket
-- request : Url -> Maybe Auth -> Request
-- request baseUrl auth =
--     let
--         request =
--             Request "GET" baseUrl [] Http.empty RootEndpoint
--     in
--         case auth of
--             Nothing ->
--                 request
--             Just auth ->
--                 request |> withAuthHeader auth


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    let
        newHeaders =
            ( name, value ) :: headers
    in
        { config | headers = newHeaders }


withAuthHeader : Auth -> Config -> Config
withAuthHeader auth config =
    case auth of
        Basic username password ->
            config
                |> withHeader
                    "Authorization"
                    ("Basic " ++ username ++ ":" ++ password)

        Bearer token ->
            config
                |> withHeader "Authorization" ("Bearer " ++ token)



-- Example usage (dream api)
-- serverInfo = request "https://kinto.dev.mozaws.net/v1" Nothing
-- getServerInfo Json.value Json.value
--getBuckets : BodyReader a -> BodyReader b -> RequestBuilder -> Task (Error a) (Response b)
--getBuckets reqBuilder =
--    get "buckets"
----
--let
--    buckets = kintoReqBuilder "base url here" auth
--        |> getBuckets errorDecoder successDecoder
--type alias Url = String
--get : String
--request =
--    HttpBuilder.get "https://kinto.dev.mozaws.net/v1/buckets/default/collections/test-items/records"
--        |> withHeader "Authorization" "Basic dGVzdDp0ZXN0"
--        |> send (jsonReader decodeRecords) stringReader
