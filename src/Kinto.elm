module Kinto
    exposing
        ( Auth(..)
        , Endpoint(..)
        , Filter(..)
        , Client
        , Resource
        , Pager
        , bucketResource
        , collectionResource
        , recordResource
        , decodeData
        , encodeData
        , ErrorDetail
        , Error(..)
        , endpointUrl
        , errorDecoder
        , toResponse
        , extractError
        , headersForAuth
        , client
        , withFilter
        , sortBy
        , limit
        , send
        , toRequest
        , get
        , getList
        , getNextList
        , create
        , update
        , replace
        , delete
        )

{-| [Kinto](http://www.kinto-storage.org/) client to ease communicating with
the REST API.

Kinto is the backend for your next application. It's a generic JSON document
store with sharing and synchronisation capabilities. It's open source. It's
easy to deploy. You can host it yourself, own your data and keep it out of
silos.


# Creating requests

You create requests on either an item or a plural (list) endpoint.
[Kinto concepts](http://kinto.readthedocs.io/en/stable/concepts.html) explain
that in details.

Plural (list) endpoints are:

  - buckets: `buckets/`
  - collections: `buckets/:bucketId/collections/`
  - records: `buckets/:bucketId/collections/:collectionId/records/`

Item endpoints are:

  - bucket: `buckets/:bucketId`
  - collection: `buckets/:bucketId/collections/:collectionId`
  - record: `buckets/:bucketId/collections/:collectionId/records/:recordId`

@docs get, getList, getNextList, create, update, replace, delete


# Sorting, limiting, filtering

@docs sortBy, limit, withFilter, Filter


# Configure a client and resources

@docs Client, client, Auth, headersForAuth, Resource, bucketResource, collectionResource, recordResource, decodeData, encodeData, errorDecoder


# Types and Errors

@docs Endpoint, endpointUrl, ErrorDetail, Error, extractError, Pager, toResponse


# Sending requests

@docs send, toRequest

-}

import Base64
import Dict
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode


type alias Url =
    String



-- Auth


{-| A type for authentication

    Basic "username" "password"
    Bearer "<token>"
    Custom "customType" "customString"

-}
type Auth
    = NoAuth
    | Basic String String
    | Bearer String
    | Custom String String



-- Kinto types


type alias BucketName =
    String


type alias CollectionName =
    String


type alias RecordId =
    String


{-| A type for Kinto API endpoints.

    RecordEndpoint "bucket-name" "collection-name" "item-id"

-}
type Endpoint
    = RootEndpoint
    | BucketListEndpoint
    | BucketEndpoint BucketName
    | CollectionListEndpoint BucketName
    | CollectionEndpoint BucketName CollectionName
    | RecordListEndpoint BucketName CollectionName
    | RecordEndpoint BucketName CollectionName RecordId


{-| A type for filtering, used with `withFilter`
-}
type Filter
    = Equal String String
    | Min String String
    | Max String String
    | LT String String
    | GT String String
    | IN String (List String)
    | NOT String String
    | LIKE String String
    | SINCE String
    | BEFORE String


type alias Body =
    Encode.Value


{-| A Kinto Client. Constructed using the `client` helper.
-}
type alias Client =
    { baseUrl : String
    , headers : List ( String, String )
    }


{-| A type for resources. Constructed using one of `bucketResource`,
`collectionResource` or `recordResource`.
-}
type alias Resource a =
    { itemEndpoint : String -> Endpoint
    , listEndpoint : Endpoint
    , itemDecoder : Decode.Decoder a
    , listDecoder : Decode.Decoder (List a)
    }


{-| A constructor for a bucket resource.

    bucketResource bucketDecoder

-}
bucketResource : Decode.Decoder a -> Resource a
bucketResource decoder =
    Resource
        (BucketEndpoint)
        (BucketListEndpoint)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


{-| A constructor for a collection resource.

    collectionResource "bucket-name" collectionDecoder

-}
collectionResource : BucketName -> Decode.Decoder a -> Resource a
collectionResource bucket decoder =
    Resource
        (CollectionEndpoint bucket)
        (CollectionListEndpoint bucket)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


{-| A constructor for a record resource.
-}
recordResource : BucketName -> CollectionName -> Decode.Decoder a -> Resource a
recordResource bucket collection decoder =
    Resource
        (RecordEndpoint bucket collection)
        (RecordListEndpoint bucket collection)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


{-| A decoder for a basic Kinto response.
-}
decodeData : Decode.Decoder a -> Decode.Decoder a
decodeData decoder =
    Decode.field "data" decoder


{-| An encoder for a basic Kinto query.
-}
encodeData : Encode.Value -> Encode.Value
encodeData encoder =
    Encode.object
        [ ( "data", encoder ) ]



-- Pagination


{-| A type for paginated results. The `nextPage` field may contain the URL to request
to retrieve the next page of results.
-}
type alias Pager a =
    { results : List a
    , nextPage : Maybe String
    }


decodePager : Decode.Decoder (List a) -> Http.Response String -> Result.Result String (Pager a)
decodePager decoder response =
    let
        decoded =
            Decode.decodeString decoder response.body

        nextPage =
            Dict.get "Next-Page" response.headers
    in
        case decoded of
            Ok decoded ->
                Ok <| Pager decoded nextPage

            Err error ->
                Err error



-- Kinto errors


{-| A type for Kinto error details.
-}
type alias ErrorDetail =
    { errno : Int
    , message : String
    , code : Int
    , error : String
    }


type alias StatusCode =
    Int


type alias StatusMsg =
    String


{-| A type for all errors that the elm-client may return.
-}
type Error
    = ServerError StatusCode StatusMsg String
    | KintoError StatusCode StatusMsg ErrorDetail
    | NetworkError Http.Error



-- Making requests


{-| Get the full url to an endpoint.

    endpointUrl "https://kinto.dev.mozaws.net/v1/" (RecordListEndpoint "default" "test-items")

-}
endpointUrl : String -> Endpoint -> Url
endpointUrl baseUrl endpoint =
    let
        url =
            if String.endsWith "/" baseUrl then
                String.dropRight 1 baseUrl
            else
                baseUrl

        joinUrl =
            String.join "/"
    in
        case endpoint of
            RootEndpoint ->
                -- Otherwise Kinto returns a 307
                -- See https://github.com/Kinto/kinto/issues/852
                url ++ "/"

            BucketListEndpoint ->
                joinUrl [ url, "buckets" ]

            BucketEndpoint bucketName ->
                joinUrl [ url, "buckets", bucketName ]

            CollectionListEndpoint bucketName ->
                joinUrl [ url, "buckets", bucketName, "collections" ]

            CollectionEndpoint bucketName collectionName ->
                joinUrl [ url, "buckets", bucketName, "collections", collectionName ]

            RecordListEndpoint bucketName collectionName ->
                joinUrl [ url, "buckets", bucketName, "collections", collectionName, "records" ]

            RecordEndpoint bucketName collectionName recordId ->
                joinUrl [ url, "buckets", bucketName, "collections", collectionName, "records", recordId ]


alwaysEncode : String -> String
alwaysEncode string =
    case Base64.encode string of
        Err msg ->
            Debug.crash "b64encoding failed" msg

        Ok hash ->
            hash



-- Dealing with answers from the Kinto server


{-| A decoder for `ErrorDetail`. This is the kind of json message answered by Kinto when there's an error:

    {"errno":104,
     "message":"Please authenticate yourself to use this endpoint.",
     "code":401,
     "error":"Unauthorized"}

-}
errorDecoder : Decode.Decoder ErrorDetail
errorDecoder =
    Decode.map4 ErrorDetail
        (Decode.field "errno" Decode.int)
        (Decode.field "message" Decode.string)
        (Decode.field "code" Decode.int)
        (Decode.field "error" Decode.string)


{-| Change the error from an `Http.Error` to an `Error`.
-}
toResponse : Result Http.Error a -> Result Error a
toResponse response =
    response
        |> Result.mapError extractError


{-| Extract an `Error` from an `Http.Error`.
-}
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



-- Helpers


{-| Return the header name and value for the given `Auth`.

    headersForAuth (Basic "username" "password")

-}
headersForAuth : Auth -> ( String, String )
headersForAuth auth =
    case auth of
        NoAuth ->
            ( "", "" )

        Basic username password ->
            ( "Authorization"
            , ("Basic " ++ (alwaysEncode (username ++ ":" ++ password)))
            )

        Bearer token ->
            ( "Authorization", ("Bearer " ++ token) )

        Custom realm token ->
            ( "Authorization", (realm ++ " " ++ token) )


{-| A constructor for a `Client`.

    client
        "https://kinto.dev.mozaws.net/v1/"
        (Basic "username" "password")

-}
client : Url -> Auth -> Client
client baseUrl auth =
    Client baseUrl [ (headersForAuth auth) ]



-- Filtering


{-| Add [filtering query parameters](http://kinto.readthedocs.io/en/stable/api/1.x/filtering.html) to the request sent to the Kinto server.

    client
        |> getList recordResource
        |> withFilter (NOT "title" "test")
        |> send TodosFetched

-}
withFilter :
    Filter
    -> HttpBuilder.RequestBuilder a
    -> HttpBuilder.RequestBuilder a
withFilter filter builder =
    let
        header =
            case filter of
                Equal key val ->
                    ( key, val )

                Min key val ->
                    ( "min_" ++ key, val )

                Max key val ->
                    ( "max_" ++ key, val )

                LT key val ->
                    ( "lt_" ++ key, val )

                GT key val ->
                    ( "gt_" ++ key, val )

                IN key values ->
                    ( "in_" ++ key, String.join "," values )

                NOT key val ->
                    ( "not_" ++ key, val )

                LIKE key val ->
                    ( "like_" ++ key, val )

                SINCE val ->
                    ( "_since", val )

                BEFORE val ->
                    ( "_before", val )
    in
        builder
            |> HttpBuilder.withQueryParams [ header ]



-- Sorting


{-| Add [sorting query parameters](http://kinto.readthedocs.io/en/stable/api/1.x/sorting.html) to the request sent to the Kinto server.

    client
        |> getList recordResource
        |> sortBy ["title", "description"]
        |> send TodosFetched

-}
sortBy :
    List String
    -> HttpBuilder.RequestBuilder a
    -> HttpBuilder.RequestBuilder a
sortBy keys builder =
    builder
        |> HttpBuilder.withQueryParams [ ( "_sort", String.join "," keys ) ]



-- Limiting


{-| Add [limit query parameters](http://kinto.readthedocs.io/en/stable/api/1.x/pagination.html) to the request sent to the Kinto server.

    client
        |> getList recordResource
        |> limit 10
        |> send TodosFetched

-}
limit :
    Int
    -> HttpBuilder.RequestBuilder a
    -> HttpBuilder.RequestBuilder a
limit perPage builder =
    builder
        |> HttpBuilder.withQueryParams [ ( "_limit", toString perPage ) ]



-- High level API


{-| Send a request to the Kinto server.

    client
        |> create resource data
        |> send TodosCreated

-}
send : (Result Error a -> msg) -> HttpBuilder.RequestBuilder a -> Cmd msg
send tagger builder =
    builder
        |> HttpBuilder.send (toResponse >> tagger)


{-| Extract the Http.Request component of the request, for introspection,
testing, or converting to a `Task` using `Http.toTask`.

    client
        |> create resource data
        |> toRequest

-}
toRequest : HttpBuilder.RequestBuilder a -> Http.Request a
toRequest builder =
    builder
        |> HttpBuilder.toRequest


{-| Create a GET request on an item endpoint

    get resource itemId

-}
get : Resource a -> String -> Client -> HttpBuilder.RequestBuilder a
get resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a GET request on one of the plural endpoints, allowing to retrieve a `Pager`
when the response is parsed.

    getList resource

-}
getList : Resource a -> Client -> HttpBuilder.RequestBuilder (Pager a)
getList resource client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectStringResponse (decodePager resource.listDecoder))


{-| Create a GET request to retrieve the next page of results.

    getNextList pager.nextPage resource

-}
getNextList : Url -> Resource a -> Client -> HttpBuilder.RequestBuilder (Pager a)
getNextList url resource client =
    url
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectStringResponse (decodePager resource.listDecoder))


{-| Create a DELETE request on an item endpoint:

    delete resource itemId

-}
delete : Resource a -> String -> Client -> HttpBuilder.RequestBuilder a
delete resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.delete
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a POST request on a plural endpoint:

    create resource data

-}
create : Resource a -> Body -> Client -> HttpBuilder.RequestBuilder a
create resource body client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.post
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a PATCH request on an item endpoint:

    update resource itemId

-}
update : Resource a -> String -> Body -> Client -> HttpBuilder.RequestBuilder a
update resource itemId body client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.patch
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a PUT request on an item endpoint:

    put resource itemId

-}
replace : Resource a -> String -> Body -> Client -> HttpBuilder.RequestBuilder a
replace resource itemId body client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.put
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)
