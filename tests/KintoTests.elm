module KintoTests exposing (all)

import Http
import Test exposing (..)
import Expect
import Kinto
    exposing
        ( client
        , endpointUrl
        , makeRequest
        , withHeader
        , withAuthHeader
        , Auth(..)
        , Endpoint(..)
        , Config
        )


config =
    Config "http://example.com" []


authConfig =
    Config
        "http://example.com"
        [ ( "Authorization", "Basic dXNlcjpwYXNz" ) ]


all : Test
all =
    describe "Kinto config module"
        [ describe "addHeader helper"
            [ test "returns the new header if previously empty" <|
                \() ->
                    Expect.equal
                        [ ( "foo", "bar" ) ]
                        (config |> withHeader "foo" "bar").headers
            , test "adds the headers to the previous list of headers" <|
                \() ->
                    Expect.equal
                        [ ( "foo", "bar" )
                        , ( "Authorization", "Basic dXNlcjpwYXNz" )
                        ]
                        (authConfig |> withHeader "foo" "bar").headers
            ]
        , describe "withAuthHeader helper"
            [ test "adds the authentication headers to an empty config" <|
                \() ->
                    Expect.equal
                        [ ( "Authorization", "Basic dXNlcjpwYXNz" ) ]
                        (config
                            |> withAuthHeader (Basic "user" "pass")
                            |> Result.withDefault config
                        ).headers
            , test "adds the authentication headers to the existing ones" <|
                \() ->
                    Expect.equal
                        [ ( "Authorization", "Basic Zm9vOmJhcg==" )
                        , ( "Authorization", "Basic dXNlcjpwYXNz" )
                        ]
                        (authConfig
                            |> withAuthHeader (Basic "foo" "bar")
                            |> Result.withDefault config
                        ).headers
            ]
        , describe "endpointUrl helper"
            (List.map
                endpointTest
                [ ( "http://example.com/", RootEndpoint )
                , ( "http://example.com/buckets"
                  , BucketEndpoint Nothing
                  )
                , ( "http://example.com/buckets/bucketName"
                  , BucketEndpoint (Just "bucketName")
                  )
                , ( "http://example.com/buckets/bucketName/collections"
                  , CollectionEndpoint "bucketName" Nothing
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName"
                  , CollectionEndpoint "bucketName" (Just "collectionName")
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records"
                  , RecordEndpoint "bucketName" "collectionName" Nothing
                  )
                , ( "http://example.com/buckets/bucketName/collections/collectionName/records/record_id"
                  , RecordEndpoint "bucketName" "collectionName" (Just "record_id")
                  )
                ]
            )
        , describe "makeRequest helper"
            [ test "creates a Http.Request for the given config, endpoint and verb" <|
                \() ->
                    Expect.equal
                        { verb = "GET"
                        , url = config.baseUrl ++ "/"
                        , headers = []
                        , body = Http.empty
                        }
                        (makeRequest config Kinto.RootEndpoint "GET")
            , test "creates a Http.Request for the given authenticated config, endpoint and verb" <|
                \() ->
                    Expect.equal
                        { verb = "POST"
                        , url = authConfig.baseUrl ++ "/buckets/bucketName/collections/collectionName/records/record_id"
                        , headers = [ ( "Authorization", "Basic dXNlcjpwYXNz" ) ]
                        , body = Http.empty
                        }
                        (makeRequest
                            authConfig
                            (Kinto.RecordEndpoint
                                "bucketName"
                                "collectionName"
                                (Just "record_id")
                            )
                            "POST"
                        )
            ]
        ]


endpointTest : ( String, Endpoint ) -> Test
endpointTest ( expected, endpoint ) =
    test (toString endpoint) <|
        \() -> Expect.equal expected (endpointUrl config endpoint)
