module Kinto exposing (..)

import Base64
import Http
import HttpBuilder
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


type alias Client =
    { baseUrl : String
    , headers : List ( String, String )
    }


type alias Resource a =
    { itemEndpoint : String -> Endpoint
    , listEndpoint : Endpoint
    , itemDecoder : Decode.Decoder a
    , listDecoder : Decode.Decoder (List a)
    }


bucketResource : Decode.Decoder a -> Resource a
bucketResource decoder =
    Resource
        (BucketEndpoint)
        (BucketListEndpoint)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


collectionResource : BucketName -> Decode.Decoder a -> Resource a
collectionResource bucket decoder =
    Resource
        (CollectionEndpoint bucket)
        (CollectionListEndpoint bucket)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


recordResource : BucketName -> CollectionName -> Decode.Decoder a -> Resource a
recordResource bucket collection decoder =
    Resource
        (RecordEndpoint bucket collection)
        (RecordListEndpoint bucket collection)
        (decodeData decoder)
        (decodeData (Decode.list decoder))


decodeData : Decode.Decoder a -> Decode.Decoder a
decodeData decoder =
    field "data" decoder



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


errorDecoder : Decode.Decoder ErrorRecord
errorDecoder =
    Decode.map4 ErrorRecord
        (field "errno" Decode.int)
        (field "message" Decode.string)
        (field "code" Decode.int)
        (field "error" Decode.string)


toKintoResponse : Result Http.Error a -> Result Error a
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



-- Helpers


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


client : Url -> Auth -> Client
client baseUrl auth =
    Client baseUrl [ (headersForAuth auth) ]



-- Filtering


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



-- High level API


send : (Result Error a -> msg) -> HttpBuilder.RequestBuilder a -> Cmd msg
send tagger builder =
    builder
        |> HttpBuilder.send (toKintoResponse >> tagger)


get : Resource a -> String -> Client -> HttpBuilder.RequestBuilder a
get resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


getList : Resource a -> Client -> HttpBuilder.RequestBuilder (List a)
getList resource client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.get
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.listDecoder)


delete : Resource a -> String -> Client -> HttpBuilder.RequestBuilder a
delete resource itemId client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.delete
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


create : Resource a -> Body -> Client -> HttpBuilder.RequestBuilder a
create resource body client =
    endpointUrl client.baseUrl resource.listEndpoint
        |> HttpBuilder.post
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)


update : Resource a -> String -> Body -> Client -> HttpBuilder.RequestBuilder a
update resource itemId body client =
    endpointUrl client.baseUrl (resource.itemEndpoint itemId)
        |> HttpBuilder.patch
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson resource.itemDecoder)
