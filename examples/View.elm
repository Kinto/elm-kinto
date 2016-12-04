module View exposing (view)

import Time exposing (Time)
import Html
import Html.Events
import Html.Attributes
import Utils
import Model exposing (Model, Record, Msg(..), Sort(..))


formatLastModified : Int -> Time -> String
formatLastModified timestamp currentTime =
    Utils.timeAgo (toFloat timestamp) currentTime


iconBtn : String -> Msg -> Html.Html Msg
iconBtn icon action =
    Html.button
        [ Html.Attributes.class "btn btn-xs btn-default"
        , Html.Events.onClick action
        ]
        [ Html.i [ Html.Attributes.class ("glyphicon glyphicon-" ++ icon) ]
            []
        ]


recordRow : Time -> Record -> Html.Html Msg
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


recordsList : List Record -> Time -> Sort -> Html.Html Msg
recordsList records currentTime sort =
    Html.table [ Html.Attributes.class "table" ]
        [ Html.thead []
            [ Html.tr []
                (recordHeaders sort)
            ]
        , Html.tbody [] (List.map (recordRow currentTime) records)
        ]


errorNotif : Maybe String -> Html.Html Msg
errorNotif error =
    case error of
        Nothing ->
            Html.text ""

        Just message ->
            Html.div [ Html.Attributes.class "alert alert-danger" ]
                [ Html.text ("Error: " ++ message) ]


view : Model -> Html.Html Msg
view { error, records, formData, currentTime, sort, limit } =
    let
        lim =
            limit
                |> Maybe.map toString
                |> Maybe.withDefault ""
    in
        Html.div [ Html.Attributes.class "container" ]
            [ Html.h1 [] [ Html.text "elm-kinto demo" ]
            , Html.small
                []
                [ Html.text "Limit records to display: "
                , Html.form
                    [ Html.Events.onSubmit Limit
                    , Html.Attributes.style [ ( "display", "inline" ) ]
                    ]
                    [ Html.input
                        [ Html.Attributes.type_ "number"
                        , Html.Attributes.min "1"
                        , Html.Attributes.value lim
                        , Html.Events.onInput NewLimit
                        , Html.Attributes.style [ ( "width", "40px" ) ]
                        ]
                        []
                    , Html.button
                        [ Html.Attributes.type_ "submit" ]
                        [ Html.text "limit" ]
                    ]
                ]
            , errorNotif error
            , recordsList records currentTime sort
            , formView formData
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
    (formVerb model)
        ++ " "
        ++ (Maybe.withDefault "" model.id)
        |> String.trim


formView : Model.FormData -> Html.Html Msg
formView formData =
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
