module KintoTests exposing (all)

import Dict
import Http
import Test exposing (..)
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto


baseUrl =
    "http://example.com/"


all : Test
all =
    describe "Kinto module"
        [ describe "headersForAuth helper"
            [ test "returns an empty header for NoAuth" <|
                \() ->
                    Expect.equal
                        ( "", "" )
                        (Kinto.headersForAuth Kinto.NoAuth)
            , test "returns Basic Auth headers for Basic" <|
                \() ->
                    Expect.equal
                        ( "Authorization", "Basic Zm9vOmJhcg==" )
                        (Kinto.headersForAuth (Kinto.Basic "foo" "bar"))
            , test "returns Bearer Auth headers for Bearer" <|
                \() ->
                    Expect.equal
                        ( "Authorization", "Bearer foobar" )
                        (Kinto.headersForAuth (Kinto.Bearer "foobar"))
            ]
        , describe "endpointUrl helper"
            (List.map
                endpointTest
                [ ( "http://example.com/", Kinto.RootEndpoint )
                , ( "http://example.com/buckets"
                  , Kinto.BucketListEndpoint
                  )
                , ( "http://example.com/buckets/bucketName"
                  , Kinto.BucketEndpoint "bucketName"
                  )
                , ( "http://example.com/buckets/bucketName/collections"
                  , Kinto.CollectionListEndpoint "bucketName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName"
                  , Kinto.CollectionEndpoint "bucketName" "collectionName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records"
                  , Kinto.RecordListEndpoint "bucketName" "collectionName"
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records/record_id"
                  , Kinto.RecordEndpoint "bucketName" "collectionName" "record_id"
                  )
                ]
            )
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
        , describe "toResponse"
            [ test "Returns the data from the server response" <|
                \() ->
                    Expect.equal
                        (Kinto.toResponse (Ok (Encode.list [])))
                        (Ok (Encode.list []))
            , test "Returns a KintoError" <|
                \() ->
                    Expect.equal
                        (Kinto.toResponse
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
                        (Kinto.toResponse
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
                        (Kinto.toResponse
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
                        (Kinto.toResponse
                            (Err <| Http.BadUrl "bad url")
                        )
                        (Err <|
                            Kinto.NetworkError (Http.BadUrl "bad url")
                        )
            , test "Returns a NetworkError in case of timeout" <|
                \() ->
                    Expect.equal
                        (Kinto.toResponse
                            (Err <| Http.Timeout)
                        )
                        (Err <|
                            Kinto.NetworkError Http.Timeout
                        )
            , test "Returns a NetworkError in case of NetworkError" <|
                \() ->
                    Expect.equal
                        (Kinto.toResponse
                            (Err <| Http.NetworkError)
                        )
                        (Err <|
                            Kinto.NetworkError Http.NetworkError
                        )
            ]
        ]


endpointTest : ( String, Kinto.Endpoint ) -> Test
endpointTest ( expected, endpoint ) =
    test (toString endpoint) <|
        \() -> Expect.equal expected (Kinto.endpointUrl baseUrl endpoint)
