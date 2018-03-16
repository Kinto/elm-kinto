module Kinto
    exposing
        ( Auth(..)
        , Endpoint(..)
        , Filter(..)
        , Client
        , Request
        , Resource
        , Pager
        , emptyPager
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
        , filter
        , sort
        , limit
        , send
        , toRequest
        , get
        , getList
        , loadNextPage
        , updatePager
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


# Configure a client and resources

@docs Client, client, Auth, headersForAuth, Resource, bucketResource, collectionResource, recordResource, decodeData, encodeData, errorDecoder


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

@docs Request


## Single item requests

@docs get, create, update, replace, delete


## Resource list requests

@docs getList


### Paginated list

@docs Pager, emptyPager, updatePager, loadNextPage


# Sorting, limiting, filtering

@docs sort, limit, filter, Filter


# Types and Errors

@docs Endpoint, endpointUrl, ErrorDetail, Error, extractError, toResponse


# Sending requests

@docs send, toRequest

-}

import Base64
import Dict
import Dict.Extra
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode


type alias Url =
    String


{-| A type describing a Kinto request. Basically an alias for an
[elm-http-builder](https://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest)
request builder.
-}
type alias Request a =
    HttpBuilder.RequestBuilder a



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


{-| A type for filtering, used with `filter`
-}
type Filter
    = EQUAL String String
    | MIN String String
    | MAX String String
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


{-| A type for a Kinto resource. Usually constructed using one of `bucketResource`,
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


{-| A stateful accumulator for a paginated list of objects.
-}
type alias Pager a =
    { client : Client
    , objects : List a
    , decoder : Decode.Decoder (List a)
    , total : Int
    , nextPage : Maybe Url
    }


{-| Initialize a `Pager`.

    emptyPager resource

-}
emptyPager : Client -> Resource a -> Pager a
emptyPager client resource =
    { client = client
    , objects = []
    , decoder = resource.listDecoder
    , total = 0
    , nextPage = Nothing
    }


{-| Update a previous pager with data from a new one, appending new objects
to the previous list.

    updatePager nextPager previousPager

-}
updatePager : Pager a -> Pager a -> Pager a
updatePager nextPager previousPager =
    { previousPager
        | total = nextPager.total
        , nextPage = nextPager.nextPage
        , objects = previousPager.objects ++ nextPager.objects
    }


{-| Decode a `Pager`.
-}
decodePager : Client -> Decode.Decoder (List a) -> Http.Response String -> Result.Result String (Pager a)
decodePager client decoder response =
    let
        headers =
            Dict.Extra.mapKeys String.toLower response.headers

        nextPage =
            Dict.get "next-page" headers

        total =
            Dict.get "total-records" headers
                |> Maybe.map (String.toInt >> Result.withDefault 0)
                |> Maybe.withDefault 0

        createPager objects =
            { client = client
            , objects = objects
            , decoder = decoder
            , total = total
            , nextPage = nextPage
            }
    in
        Decode.decodeString decoder response.body
            |> Result.map createPager



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
            ( "Authorization", "" )

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

    type Msg = TodosFetched (Result Kinto.Error (Kinto.pager Todo))

    client
        |> getList recordResource
        |> filter (NOT "title" "test")
        |> send TodosFetched

-}
filter :
    Filter
    -> Request a
    -> Request a
filter filter builder =
    let
        header =
            case filter of
                EQUAL key val ->
                    ( key, val )

                MIN key val ->
                    ( "min_" ++ key, val )

                MAX key val ->
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

    type Msg = TodosFetched (Result Kinto.Error (Kinto.pager Todo))

    client
        |> getList recordResource
        |> sort ["title", "description"]
        |> send TodosFetched

-}
sort :
    List String
    -> Request a
    -> Request a
sort keys builder =
    builder
        |> HttpBuilder.withQueryParams [ ( "_sort", String.join "," keys ) ]



-- Limiting


{-| Add [limit query parameters](http://kinto.readthedocs.io/en/stable/api/1.x/pagination.html) to the request sent to the Kinto server.

    type Msg = TodosFetched (Result Kinto.Error (Kinto.pager Todo))

    client
        |> getList recordResource
        |> limit 10
        |> send TodosFetched

-}
limit :
    Int
    -> Request a
    -> Request a
limit perPage builder =
    builder
        |> HttpBuilder.withQueryParams [ ( "_limit", toString perPage ) ]



-- High level API


{-| Send a request to the Kinto server.

    type Msg = TodoAdded (Result Kinto.Error Todo)

    client
        |> create resource data
        |> send TodoAdded

-}
send : (Result Error a -> msg) -> Request a -> Cmd msg
send tagger builder =
    builder
        |> HttpBuilder.send (toResponse >> tagger)


{-| Extract the Http.Request component of the request, for introspection,
testing, or converting to a `Task` using `Http.toTask`.

    client
        |> create resource data
        |> toRequest

-}
toRequest : Request a -> Http.Request a
toRequest builder =
    builder
        |> HttpBuilder.toRequest


{-| Create a GET request on an item endpoint

    client
        |> get resource itemId

-}
get : Resource a -> String -> Client -> Request a
get resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a GET request on one of the plural endpoints. As lists are always
possibly paginated, When the request is succesful, a `Pager` is attached to the
reponse message.

    client
        |> getList resource

-}
getList : Resource a -> Client -> HttpBuilder.RequestBuilder (Pager a)
getList resource client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect
            (Http.expectStringResponse (decodePager client resource.listDecoder))


{-| If a pager has a `nextPage`, creates a GET request to retrieve the next page of objects.
When the request is succesful, a `Pager` with new objects appended is attached to the
reponse message.

    client
        |> loadNextPage pager

-}
loadNextPage : Pager a -> Maybe (HttpBuilder.RequestBuilder (Pager a))
loadNextPage pager =
    case pager.nextPage of
        Just nextPage ->
            nextPage
                |> HttpBuilder.get
                |> HttpBuilder.withHeaders pager.client.headers
                |> HttpBuilder.withExpect
                    (Http.expectStringResponse (decodePager pager.client pager.decoder))
                |> Just

        Nothing ->
            Nothing


{-| Create a DELETE request on an item endpoint:

    client
        |> delete resource itemId

-}
delete : Resource a -> String -> Client -> Request a
delete resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.delete
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a POST request on a plural endpoint:

    client
        |> create resource itemId data

-}
create : Resource a -> Body -> Client -> Request a
create resource body client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.post
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a PATCH request on an item endpoint:

    client
        |> update resource itemId data

-}
update : Resource a -> String -> Body -> Client -> Request a
update resource itemId body client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.patch
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


{-| Create a PUT request on an item endpoint:

    client
        |> replace resource itemId data

-}
replace : Resource a -> String -> Body -> Client -> Request a
replace resource itemId body client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.put
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody (encodeData body)
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)
