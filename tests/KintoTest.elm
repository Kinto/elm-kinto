module KintoTest exposing (all)

import Dict
import Expect
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Test exposing (..)


baseUrl : String
baseUrl =
    "http://example.com/"


all : Test
all =
    describe "Kinto module"
        [ describe "headersForAuth helper"
            [ test "returns an empty header for NoAuth" <|
                \() ->
                    Expect.equal
                        ( "Authorization", "" )
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
            , test "returns Custom Portier Auth headers for Portier" <|
                \() ->
                    Expect.equal
                        ( "Authorization", "Portier foobar" )
                        (Kinto.headersForAuth (Kinto.Custom "Portier" "foobar"))
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
        , describe "withQueryParam helper"
            [ test "Calling withQueryParam with no query in request url adds one" <|
                \() ->
                    let
                        newUrl =
                            HttpBuilder.get "http://www.perdu.com/"
                                |> Kinto.withQueryParam ( "_sort", "-last_modified" )
                                |> .url
                    in
                    Expect.equal newUrl "http://www.perdu.com/?_sort=-last_modified"
            , test "Calling withQueryParam with existing query keeps both the old and new ones" <|
                \() ->
                    let
                        newUrl =
                            HttpBuilder.get "http://www.perdu.com/?is_active=true"
                                |> Kinto.withQueryParam ( "_sort", "-last_modified" )
                                |> Kinto.withQueryParam ( "_limit", "20" )
                                |> Kinto.withQueryParam ( "name", "Rémy Hubscher" )
                                |> .url
                    in
                    Expect.equal newUrl "http://www.perdu.com/?name=R%C3%A9my+Hubscher&_limit=20&_sort=-last_modified&is_active=true"
            ]
        ]


endpointTest : ( String, Kinto.Endpoint ) -> Test
endpointTest ( expected, endpoint ) =
    test (Kinto.endpointUrl baseUrl endpoint) <|
        \() -> Expect.equal expected (Kinto.endpointUrl baseUrl endpoint)
