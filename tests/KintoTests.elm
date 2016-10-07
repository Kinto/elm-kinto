module KintoTests exposing (all)

import Http
import Test exposing (..)
import Expect
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
                        [ ( "foo", "bar" ) ]
                        (config |> withHeader "foo" "bar").headers
            , test "adds the headers to the previous list of headers" <|
                \() ->
                    Expect.equal
                        [ ( "baz", "crux" )
                        , ( "foo", "bar" )
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
                        [ ( "Authorization", "Basic dXNlcjpwYXNz" ) ]
                        (headersFromConfig authConfig)
            , test "adds the authentication headers to the existing ones" <|
                \() ->
                    Expect.equal
                        [ ( "Authorization", "Basic dXNlcjpwYXNz" )
                        , ( "foo", "bar" )
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
        ]


endpointTest : ( String, Endpoint ) -> Test
endpointTest ( expected, endpoint ) =
    test (toString endpoint) <|
        \() -> Expect.equal expected (endpointUrl config endpoint)
