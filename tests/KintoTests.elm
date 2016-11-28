module KintoTests exposing (all)

import Dict
import Http
import Test exposing (..)
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
    exposing
        ( configure
        , endpointUrl
        , withHeader
        , headersFromConfig
        , Auth(..)
        , Endpoint(..)
        , Config
        )


config =
    configure "http://example.com" NoAuth


authConfig =
    configure "http://example.com" (Basic "user" "pass")


all : Test
all =
    describe "Kinto config module"
        [ describe "addHeader helper"
            [ test "returns the new header if previously empty" <|
                \() ->
                    Expect.equal
                        [ (Http.header "foo" "bar") ]
                        (config |> withHeader "foo" "bar").headers
            , test "adds the headers to the previous list of headers" <|
                \() ->
                    Expect.equal
                        [ (Http.header "baz" "crux")
                        , (Http.header "foo" "bar")
                        ]
                        (config
                            |> withHeader "foo" "bar"
                            |> withHeader "baz" "crux"
                        ).headers
            ]
        , describe "headersFromConfig helper"
            [ test "adds the authentication headers to an empty config" <|
                \() ->
                    Expect.equal
                        [ (Http.header "Authorization" "Basic dXNlcjpwYXNz") ]
                        (headersFromConfig authConfig)
            , test "adds the authentication headers to the existing ones" <|
                \() ->
                    Expect.equal
                        [ (Http.header "Authorization" "Basic dXNlcjpwYXNz")
                        , (Http.header "foo" "bar")
                        ]
                        (headersFromConfig (withHeader "foo" "bar" authConfig))
            ]
        , describe "endpointUrl helper"
            (List.map
                endpointTest
                [ ( "http://example.com/", RootEndpoint )
                , ( "http://example.com/buckets"
                  , BucketListEndpoint
                  )
                , ( "http://example.com/buckets/bucketName"
                  , BucketEndpoint "bucketName"
                  )
                , ( "http://example.com/buckets/bucketName/collections"
                  , CollectionListEndpoint "bucketName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName"
                  , CollectionEndpoint "bucketName" "collectionName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records"
                  , RecordListEndpoint "bucketName" "collectionName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records/record_id"
                  , RecordEndpoint "bucketName" "collectionName" "record_id"
                  )
                ]
            )
        , describe "bodyDataDecoder"
            [ test "Properly decodes a Kinto response" <|
                \() ->
                    Expect.equal
                        (Decode.decodeString Kinto.bodyDataDecoder """{"data": []}""")
                        (Ok (Encode.list []))
            ]
        , describe "errorDecoder"
            [ test "Properly decodes a Kinto error" <|
                \() ->
                    Expect.equal
                        (Decode.decodeString
                            Kinto.errorDecoder
                            """{"errno": 123,
                                "message": "something failed",
                                "code": 321,
                                "error": "some error"}"""
                        )
                        (Ok <|
                            Kinto.ErrorRecord
                                123
                                "something failed"
                                321
                                "some error"
                        )
            ]
        , describe "toKintoResponse"
            [ test "Returns the data from the server response" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse (Ok (Encode.list [])))
                        (Ok (Encode.list []))
            , test "Returns a KintoError" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <|
                                Http.BadStatus
                                    (Http.Response
                                        "http://example.com"
                                        { code = 403, message = "Forbidden" }
                                        Dict.empty
                                        """{"errno":121,
                                            "message":"This user cannot access this resource.",
                                            "code":403,
                                            "error":"Forbidden"}"""
                                    )
                            )
                        )
                        (Err <|
                            Kinto.KintoError
                                403
                                "Forbidden"
                                (Kinto.ErrorRecord
                                    121
                                    "This user cannot access this resource."
                                    403
                                    "Forbidden"
                                )
                        )
            , test "Returns ServerError when we can't decode a KintoError" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <|
                                Http.BadStatus
                                    (Http.Response
                                        "http://example.com"
                                        { code = 403, message = "Forbidden" }
                                        Dict.empty
                                        """{"not-a":"kinto error"}"""
                                    )
                            )
                        )
                        (Err <|
                            Kinto.ServerError
                                403
                                "Forbidden"
                                """Expecting an object with a field named `errno` but instead got: {"not-a":"kinto error"}"""
                        )
            , test "Returns a ServerError when we get a bad payload" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <|
                                Http.BadPayload
                                    "Bad Payload"
                                    (Http.Response
                                        "http://example.com"
                                        { code = 200, message = "OK" }
                                        Dict.empty
                                        """Some bad payload"""
                                    )
                            )
                        )
                        (Err <|
                            Kinto.ServerError
                                200
                                "OK"
                                """failed decoding json: Bad Payload

Body received from server: Some bad payload"""
                        )
            , test "Returns a NetworkError in case of bad url" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <| Http.BadUrl "bad url")
                        )
                        (Err <|
                            Kinto.NetworkError (Http.BadUrl "bad url")
                        )
            , test "Returns a NetworkError in case of timeout" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <| Http.Timeout)
                        )
                        (Err <|
                            Kinto.NetworkError Http.Timeout
                        )
            , test "Returns a NetworkError in case of NetworkError" <|
                \() ->
                    Expect.equal
                        (Kinto.toKintoResponse
                            (Err <| Http.NetworkError)
                        )
                        (Err <|
                            Kinto.NetworkError Http.NetworkError
                        )
            ]
        ]


endpointTest : ( String, Endpoint ) -> Test
endpointTest ( expected, endpoint ) =
    test (toString endpoint) <|
        \() -> Expect.equal expected (endpointUrl config endpoint)
