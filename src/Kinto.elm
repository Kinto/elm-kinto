module Kinto exposing (..)

import Http


type alias Url =
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
    | BuckedEndpoint BucketName
    | CollectionEndpoint BucketName CollectionName
    | RecordEndpoint BucketName CollectionName RecordId


type alias Request =
    { verb : String
    , url : String
    , headers : List ( String, String )
    , body : Http.Body
    , endpoint : Endpoint
    }


kintoBaseUrl =
    "https://kinto.dev.mozaws.net/v1/"



-- Kinto implementation


client : Url -> Maybe Auth -> Request
client baseUrl auth =
    let
        request =
            Request "GET" baseUrl [] Http.empty RootEndpoint
    in
        case auth of
            Nothing ->
                request

            Just auth ->
                request |> withAuthHeader auth


withHeader : String -> String -> Request -> Request
withHeader name value ({ headers } as request) =
    let
        newHeaders =
            ( name, value ) :: headers
    in
        { request | headers = newHeaders }


withAuthHeader : Auth -> Request -> Request
withAuthHeader auth request =
    case auth of
        Basic username password ->
            request
                |> withHeader
                    "Authorization"
                    ("Basic " ++ username ++ ":" ++ password)

        Bearer token ->
            request
                |> withHeader "Authorization" ("Bearer " ++ token)



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
