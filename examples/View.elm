module View exposing (view)

import Html
import Html.Attributes
import Html.Events
import Kinto
import Model exposing (Model, Msg(..), Record, Sort(..))
import Time exposing (Posix)
import Utils


formatLastModified : Int -> Posix -> String
formatLastModified timestamp currentTime =
    Utils.timeAgo (Time.millisToPosix timestamp) currentTime


iconBtn : String -> Msg -> Html.Html Msg
iconBtn icon action =
    Html.button
        [ Html.Attributes.class "btn btn-xs btn-default"
        , Html.Events.onClick action
        ]
        [ Html.i [ Html.Attributes.class ("glyphicon glyphicon-" ++ icon) ]
            []
        ]


recordRow : Posix -> Record -> Html.Html Msg
recordRow currentTime { id, title, description, last_modified } =
    Html.tr []
        [ Html.td [] [ Html.text id ]
        , Html.td [] [ Html.text (Maybe.withDefault "[empty]" title) ]
        , Html.td [] [ Html.text (Maybe.withDefault "[empty]" description) ]
        , Html.td [] [ Html.text (formatLastModified last_modified currentTime) ]
        , Html.td []
            [ iconBtn "edit" (EditRecord id)
            , Html.text " "
            , iconBtn "trash" (DeleteRecord id)
            ]
        ]


headingWithSortIcon : String -> String -> String -> Html.Html Msg
headingWithSortIcon icon sortColumn heading =
    let
        content =
            if heading == sortColumn then
                [ Html.i [ Html.Attributes.class icon ] []
                , Html.text <| " " ++ heading
                ]

            else
                [ Html.text heading ]
    in
    Html.th [ Html.Events.onClick (SortByColumn heading) ] content


recordHeaders : Sort -> List (Html.Html Msg)
recordHeaders sort =
    let
        ( icon, sortColumn ) =
            case sort of
                Asc column ->
                    ( "glyphicon glyphicon-sort-by-attributes", column )

                Desc column ->
                    ( "glyphicon glyphicon-sort-by-attributes-alt", column )

        headings =
            [ "id", "title", "description", "last_modified" ]
    in
    List.map
        (headingWithSortIcon icon sortColumn)
        headings


recordsList : Maybe (Kinto.Pager Record) -> Posix -> Sort -> Html.Html Msg
recordsList maybePager currentTime sort =
    case maybePager of
        Just pager ->
            Html.div []
                [ Html.table [ Html.Attributes.class "table" ]
                    [ Html.thead []
                        [ Html.tr []
                            (recordHeaders sort)
                        ]
                    , Html.tbody [] (List.map (recordRow currentTime) pager.objects)
                    ]
                , case pager.nextPage of
                    Just nextPage ->
                        Html.button
                            [ Html.Attributes.style "display" "block"
                            , Html.Attributes.style "width" "100%"
                            , Html.Events.onClick FetchNextRecords
                            ]
                            [ Html.text "Load more" ]

                    Nothing ->
                        Html.text ""
                , Html.br [] []
                ]

        Nothing ->
            Html.text ""


errorNotif : Maybe String -> Html.Html Msg
errorNotif error =
    case error of
        Nothing ->
            Html.text ""

        Just message ->
            Html.div [ Html.Attributes.class "alert alert-danger" ]
                [ Html.text ("Error: " ++ message) ]


view : Model -> Html.Html Msg
view { maybeError, maybeClient, maybePager, formData, clientFormData, currentTime, sort, maybeLimit } =
    let
        limit =
            maybeLimit
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
    in
    Html.div [ Html.Attributes.class "container" ]
        [ Html.h1 [] [ Html.text "elm-kinto demo" ]
        , case maybeClient of
            Just _ ->
                Html.text ""

            Nothing ->
                clientFormView clientFormData
        , Html.p
            []
            [ Html.text "Limit records to display: "
            , Html.form
                [ Html.Events.onSubmit Limit
                , Html.Attributes.style "display" "inline"
                ]
                [ Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.min "1"
                    , Html.Attributes.value limit
                    , Html.Events.onInput NewLimit
                    , Html.Attributes.style "width" "40px"
                    ]
                    []
                , Html.button
                    [ Html.Attributes.type_ "submit" ]
                    [ Html.text "limit" ]
                ]
            ]
        , errorNotif maybeError
        , case maybePager of
            Just pager ->
                Html.p []
                    [ Html.text <| String.fromInt pager.total ++ " records in this collection." ]

            Nothing ->
                Html.text ""
        , recordsList maybePager currentTime sort
        , recordFormView formData
        ]


formVerb : Model.FormData -> String
formVerb { id } =
    case id of
        Nothing ->
            "Create"

        Just _ ->
            "Update"


formTitle : Model.FormData -> String
formTitle model =
    formVerb model
        ++ " "
        ++ Maybe.withDefault "" model.id
        |> String.trim


clientFormView : Model.ClientFormData -> Html.Html Msg
clientFormView clientFormData =
    Html.form [ Html.Events.onSubmit SaveClient ]
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Kinto client configuration" ]
            , Html.div [ Html.Attributes.class "form-group" ]
                [ Html.label
                    [ Html.Attributes.for "server" ]
                    [ Html.text "Kinto server" ]
                , Html.input
                    [ Html.Attributes.id "server"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value clientFormData.server
                    , Html.Events.onInput UpdateClientServer
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "form-group" ]
                [ Html.label
                    [ Html.Attributes.for "username" ]
                    [ Html.text "Username" ]
                , Html.input
                    [ Html.Attributes.id "username"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value clientFormData.username
                    , Html.Events.onInput UpdateClientUsername
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "form-group" ]
                [ Html.label
                    [ Html.Attributes.for "password" ]
                    [ Html.text "Password" ]
                , Html.input
                    [ Html.Attributes.id "password"
                    , Html.Attributes.type_ "password"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value clientFormData.password
                    , Html.Events.onInput UpdateClientPassword
                    ]
                    []
                ]
            , Html.button [ Html.Attributes.class "btn btn-primary" ]
                [ Html.text "Configure client" ]
            ]
        ]


recordFormView : Model.FormData -> Html.Html Msg
recordFormView formData =
    Html.form [ Html.Events.onSubmit Submit ]
        [ Html.fieldset []
            [ Html.legend [] [ Html.text (formTitle formData) ]
            , Html.div [ Html.Attributes.class "form-group" ]
                [ Html.label
                    [ Html.Attributes.for "title" ]
                    [ Html.text "Title" ]
                , Html.input
                    [ Html.Attributes.id "title"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value formData.title
                    , Html.Events.onInput UpdateFormTitle
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "form-group" ]
                [ Html.label
                    [ Html.Attributes.for "description" ]
                    [ Html.text "Description" ]
                , Html.textarea
                    [ Html.Attributes.id "description"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value formData.description
                    , Html.Events.onInput UpdateFormDescription
                    ]
                    []
                ]
            , Html.div []
                [ Html.button
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "btn btn-default"
                    ]
                    [ Html.text (formVerb formData) ]
                ]
            ]
        ]
