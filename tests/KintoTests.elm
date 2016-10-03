module KintoTests exposing (all)

import Http
import Test exposing (..)
import Expect
import Kinto
    exposing
        ( client
        , withHeader
        , withAuthHeader
        , Auth(..)
        , Endpoint(..)
        , Request
        )


request =
    Request "GET" "http://example.com" [] Http.empty RootEndpoint


authRequest =
    Request
        "GET"
        "http://example.com"
        [ ( "Authorization", "Basic user:pass" ) ]
        Http.empty
        RootEndpoint


all : Test
all =
    describe "Kinto request module"
        [ describe "addHeader helper"
            [ test "returns the new header if previously empty" <|
                \() ->
                    Expect.equal
                        [ ( "foo", "bar" ) ]
                        (request |> withHeader "foo" "bar").headers
            , test "adds the headers to the previous list of headers" <|
                \() ->
                    Expect.equal
                        [ ( "foo", "bar" )
                        , ( "Authorization", "Basic user:pass" )
                        ]
                        (authRequest |> withHeader "foo" "bar").headers
            ]
        , describe "withAuthHeader helper"
            [ test "adds the authentication headers to an empty request" <|
                \() ->
                    Expect.equal
                        [ ( "Authorization", "Basic user:pass" ) ]
                        (request
                            |> withAuthHeader (Basic "user" "pass")
                        ).headers
            , test "adds the authentication headers to the existing ones" <|
                \() ->
                    Expect.equal
                        [ ( "Authorization", "Basic foo:bar" )
                        , ( "Authorization", "Basic user:pass" )
                        ]
                        (authRequest
                            |> withAuthHeader (Basic "foo" "bar")
                        ).headers
            ]
        , describe "client helper"
            [ test "creates a default unauthorized request" <|
                \() ->
                    Expect.equal
                        request
                        (client "http://example.com" Nothing)
            , test "creates a default authorized request" <|
                \() ->
                    Expect.equal
                        authRequest
                        (client
                            "http://example.com"
                            (Just (Basic "user" "pass"))
                        )
            ]
        ]
