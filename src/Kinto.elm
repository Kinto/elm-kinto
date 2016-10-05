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


type Error
    = ConfigError String
    | ServerError Http.RawError
    | KintoError String



-- Kinto implementation


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
                baseUrl ++ "/"

            -- Otherwise Kinto returns a 307
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


kintoRecordDecoder : Json.Decoder Json.Value
kintoRecordDecoder =
    ("data" := Json.value)


detectKintoError : Http.Response -> Task Error Json.Value
detectKintoError response =
    case response.value of
        Http.Text body ->
            let
                responseResult = Json.decodeString kintoRecordDecoder body
            in
                case responseResult of
                    Ok data ->
                        Task.succeed data

                    Err _ ->
                        -- We failed to decode an "ErrorRecord", so we have a success
                        Task.fail (KintoError body)
        Http.Blob _ ->
            Debug.crash "We shouldn't have a blob in 0.17!"


client : Config -> Endpoint -> Verb -> Task Error Json.Value
client config endpoint verb =
    let
        request =
            makeRequest config endpoint verb
    in
        Task.andThen
            (Task.mapError
                -- TODO: Check if there's a ErrorRecord returned, if so use it
                ServerError -- Transform the "Http.RawError into a ServerError
                (Http.send Http.defaultSettings request))
            detectKintoError


withHeader : String -> String -> Config -> Config
withHeader name value ({ headers } as config) =
    let
        newHeaders =
            ( name, value ) :: headers
    in
        { config | headers = newHeaders }


basicAuthHash : Username -> Password -> Result String String
basicAuthHash user pass =
    let
        token =
            user ++ ":" ++ pass
    in
        Base64.encode token


withAuthHeader : Auth -> Config -> Result String Config
withAuthHeader auth config =
    case auth of
        NoAuth ->
            Ok config

        Basic username password ->
            let
                hash =
                    basicAuthHash username password
            in
                case hash of
                    Err error ->
                        Err ("Failed to encode credentials: " ++ error)

                    Ok hash ->
                        Ok
                            (config
                                |> withHeader "Authorization" ("Basic " ++ hash)
                            )

        Bearer token ->
            Ok (config |> withHeader "Authorization" ("Bearer " ++ token))


configure : Url -> Auth -> Result Error Config
configure baseUrl auth =
    let
        config =
            Config baseUrl []
                |> withAuthHeader auth
                |> Debug.log "Kinto request"
    in
        case config of
            Err error ->
                Err (ConfigError error)

            Ok config ->
                Ok config


getRecordList : Result Error Config -> BucketName -> CollectionName -> Task Error Json.Value
getRecordList config bucket collection =
    let
        endpoint =
            RecordEndpoint bucket collection Nothing
    in
        case config of
            Err error ->
                Task.fail error

            Ok config ->
                client config endpoint "GET"
