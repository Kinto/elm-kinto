module Model exposing
    ( ClientFormData
    , Flags
    , FormData
    , Model
    , Msg(..)
    , Record
    , Sort(..)
    , init
    , subscriptions
    , update
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Time exposing (Posix)


type alias RecordId =
    String


type alias Record =
    { id : RecordId
    , title : Maybe String
    , description : Maybe String
    , last_modified : Int
    }


type alias FormData =
    { id : Maybe String
    , title : String
    , description : String
    }


type alias ClientFormData =
    { server : String
    , username : String
    , password : String
    }


type alias Model =
    { maybeError : Maybe String
    , maybeClient : Maybe Kinto.Client
    , maybePager : Maybe (Kinto.Pager Record)
    , formData : FormData
    , clientFormData : ClientFormData
    , currentTime : Posix
    , sort : Sort
    , maybeLimit : Maybe Int
    , totalRecords : Int
    }


type Sort
    = Asc String
    | Desc String


type Msg
    = NoOp
      -- Use by TimeAgo to display human friendly timestamps
    | Tick Posix
      -- Kinto API requests
    | CountRecordsResponse (Result Kinto.Error Int)
    | FetchRecordResponse (Result Kinto.Error Record)
    | FetchRecords
    | FetchNextRecords
    | FetchRecordsResponse (Result Kinto.Error (Kinto.Pager Record))
    | CreateRecordResponse (Result Kinto.Error Record)
    | EditRecord RecordId
    | EditRecordResponse (Result Kinto.Error Record)
    | DeleteRecord RecordId
    | DeleteRecordResponse (Result Kinto.Error Record)
      -- Client Form
    | UpdateClientServer String
    | UpdateClientUsername String
    | UpdateClientPassword String
    | SaveClient
      -- Form
    | UpdateFormTitle String
    | UpdateFormDescription String
    | Submit
      -- Sorting
    | SortByColumn String
      -- Limiting
    | NewLimit String
    | Limit


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , fetchData initialModel
    )


initialFormData : FormData
initialFormData =
    FormData Nothing "" ""


initialClientFormData : ClientFormData
initialClientFormData =
    { server = "https://kinto.dev.mozaws.net/v1/"
    , username = "test"
    , password = "test"
    }


initialModel : Model
initialModel =
    { maybeError = Nothing
    , maybeClient = Nothing
    , maybePager = Nothing
    , formData = initialFormData
    , clientFormData = initialClientFormData
    , currentTime = Time.millisToPosix 0
    , sort = Desc "last_modified"
    , maybeLimit = Just 5
    , totalRecords = 0
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clientFormData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        CountRecordsResponse (Ok totalRecords) ->
            let
                _ =
                    Debug.log "total" totalRecords
            in
            ( { model | totalRecords = totalRecords }, Cmd.none )

        CountRecordsResponse (Err error) ->
            model |> updateError error

        FetchRecords ->
            ( { model
                | maybePager =
                    case model.maybeClient of
                        Just client ->
                            Just <| Kinto.emptyPager client recordResource

                        Nothing ->
                            Nothing
                , maybeError = Nothing
              }
            , fetchData model
            )

        FetchNextRecords ->
            ( { model | maybeError = Nothing }
            , case model.maybePager of
                Just pager ->
                    fetchNextRecordList pager

                Nothing ->
                    Cmd.none
            )

        FetchRecordResponse (Ok record) ->
            ( { model
                | formData = recordToFormData record
                , maybeError = Nothing
              }
            , countRecords model
            )

        FetchRecordResponse (Err error) ->
            model |> updateError error

        FetchRecordsResponse (Ok newPager) ->
            ( { model
                | maybePager =
                    case model.maybePager of
                        Just pager ->
                            Just <| Kinto.updatePager newPager pager

                        Nothing ->
                            Just <| newPager
                , maybeError = Nothing
              }
            , Cmd.none
            )

        FetchRecordsResponse (Err error) ->
            model |> updateError error

        CreateRecordResponse (Ok record) ->
            ( { model
                | maybePager = model.maybePager |> Maybe.map (addRecordToPager record)
                , maybeError = Nothing
              }
            , countRecords model
            )

        CreateRecordResponse (Err error) ->
            model |> updateError error

        EditRecord recordId ->
            ( model, fetchRecord model.maybeClient recordId )

        EditRecordResponse (Ok _) ->
            ( model, fetchData model )

        EditRecordResponse (Err error) ->
            model |> updateError error

        DeleteRecord recordId ->
            ( model, deleteRecord model.maybeClient recordId )

        DeleteRecordResponse (Ok record) ->
            ( { model
                | maybePager =
                    case model.maybePager of
                        Just pager ->
                            Just <| removeRecordFromPager record pager

                        Nothing ->
                            Nothing
                , maybeError = Nothing
              }
            , fetchData model
            )

        DeleteRecordResponse (Err error) ->
            model |> updateError error

        UpdateFormTitle title ->
            let
                formData =
                    model.formData

                updated =
                    { formData | title = title }
            in
            ( { model
                | formData = updated
                , maybePager =
                    case model.maybePager of
                        Just pager ->
                            Just <| updateRecordInPager updated pager

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        UpdateFormDescription description ->
            let
                formData =
                    model.formData

                updated =
                    { formData | description = description }
            in
            ( { model
                | formData = updated
                , maybePager =
                    case model.maybePager of
                        Just pager ->
                            Just <| updateRecordInPager updated pager

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        Submit ->
            ( { model | formData = initialFormData }
            , sendFormData model.maybeClient model.formData
            )

        SortByColumn column ->
            let
                sort =
                    case model.sort of
                        Asc sortedColumn ->
                            if sortedColumn == column then
                                Desc sortedColumn

                            else
                                Asc column

                        Desc sortedColumn ->
                            if sortedColumn == column then
                                Asc sortedColumn

                            else
                                Asc column

                updated =
                    { model | sort = sort, maybePager = Nothing }
            in
            ( updated, fetchData updated )

        NewLimit newLimit ->
            ( { model | maybeLimit = String.toInt newLimit }, Cmd.none )

        Limit ->
            ( { model | maybePager = Nothing }, fetchData model )

        UpdateClientServer server ->
            ( { model | clientFormData = { clientFormData | server = server } }, Cmd.none )

        UpdateClientUsername username ->
            ( { model | clientFormData = { clientFormData | username = username } }, Cmd.none )

        UpdateClientPassword password ->
            ( { model | clientFormData = { clientFormData | password = password } }, Cmd.none )

        SaveClient ->
            let
                client =
                    Kinto.client
                        clientFormData.server
                        (Kinto.Basic clientFormData.username clientFormData.password)
                        |> Just

                newModel =
                    { model | maybePager = Nothing, maybeClient = client }
            in
            ( newModel, fetchData newModel )


updateError : Kinto.Error -> Model -> ( Model, Cmd Msg )
updateError error model =
    ( { model | maybeError = Just <| Kinto.errorToString error }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- Helpers


recordToFormData : Record -> FormData
recordToFormData { id, title, description } =
    FormData
        (Just id)
        (Maybe.withDefault "" title)
        (Maybe.withDefault "" description)


encodeFormData : FormData -> Encode.Value
encodeFormData { title, description } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "description", Encode.string description )
        ]


addRecordToPager : Record -> Kinto.Pager Record -> Kinto.Pager Record
addRecordToPager formData pager =
    let
        updated =
            pager.objects |> List.append [ formData ]
    in
    { pager | objects = updated }


removeRecordFromPager : Record -> Kinto.Pager Record -> Kinto.Pager Record
removeRecordFromPager { id } pager =
    { pager | objects = List.filter (\record -> record.id /= id) pager.objects }


updateRecordInPager : FormData -> Kinto.Pager Record -> Kinto.Pager Record
updateRecordInPager formData pager =
    -- This enables live reflecting ongoing form updates in the records list
    case formData.id of
        Nothing ->
            pager

        Just id ->
            { pager
                | objects =
                    List.map
                        (\record ->
                            if record.id == id then
                                updateRecord formData record

                            else
                                record
                        )
                        pager.objects
            }


updateRecord : FormData -> Record -> Record
updateRecord formData record =
    { record
        | title = Just formData.title
        , description = Just formData.description
    }


recordResource : Kinto.Resource Record
recordResource =
    Kinto.recordResource "default" "test-items" decodeRecord


decodeRecord : Decode.Decoder Record
decodeRecord =
    Decode.map4 Record
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "last_modified" Decode.int)



-- Kinto API calls


fetchRecord : Maybe Kinto.Client -> RecordId -> Cmd Msg
fetchRecord maybeClient recordId =
    case maybeClient of
        Just client ->
            client
                |> Kinto.get recordResource recordId FetchRecordResponse
                |> Kinto.send

        Nothing ->
            Cmd.none


fetchNextRecordList : Kinto.Pager Record -> Cmd Msg
fetchNextRecordList pager =
    case Kinto.loadNextPage pager FetchRecordsResponse of
        Just request ->
            Kinto.send request

        Nothing ->
            Cmd.none


fetchData : Model -> Cmd Msg
fetchData model =
    Cmd.batch [ fetchRecordList model, countRecords model ]


countRecords : Model -> Cmd Msg
countRecords { maybeClient } =
    case maybeClient of
        Just client ->
            client
                |> Kinto.count recordResource CountRecordsResponse
                |> Kinto.send

        Nothing ->
            Cmd.none


fetchRecordList : Model -> Cmd Msg
fetchRecordList { maybeClient, sort, maybeLimit } =
    let
        sortColumn =
            case sort of
                Asc column ->
                    column

                Desc column ->
                    "-" ++ column

        limiter builder =
            case maybeLimit of
                Just limit ->
                    builder
                        |> Kinto.limit limit

                Nothing ->
                    builder
    in
    case maybeClient of
        Just client ->
            client
                |> Kinto.getList recordResource FetchRecordsResponse
                |> Kinto.sort [ sortColumn ]
                |> limiter
                |> Kinto.send

        Nothing ->
            Cmd.none


deleteRecord : Maybe Kinto.Client -> RecordId -> Cmd Msg
deleteRecord maybeClient recordId =
    case maybeClient of
        Just client ->
            client
                |> Kinto.delete recordResource recordId DeleteRecordResponse
                |> Kinto.send

        Nothing ->
            Cmd.none


sendFormData : Maybe Kinto.Client -> FormData -> Cmd Msg
sendFormData maybeClient formData =
    let
        data =
            encodeFormData formData
    in
    case ( maybeClient, formData.id ) of
        ( Just client, Nothing ) ->
            client
                |> Kinto.create recordResource data CreateRecordResponse
                |> Kinto.send

        ( Just client, Just recordId ) ->
            client
                |> Kinto.update recordResource recordId data EditRecordResponse
                |> Kinto.send

        _ ->
            Cmd.none
