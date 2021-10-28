module Main exposing (..)

import Browser
import Html exposing (Html, button, div, footer, header, section, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


init : () -> ( Model, Cmd Msg )
init _ =
    ( Ready, Cmd.none )


type Model
    = Ready
    | Loading
    | TodoLoaded Data
    | TodoLoadFailed


type Msg
    = Noop
    | Gettodos
    | Gottodos (Result Http.Error Data)


type alias Todo =
    { id : Int
    , title : String
    , description : String
    , completion_status : Bool
    }


type alias Data =
    { data : List Todo
    }


todoparser : JD.Decoder Todo
todoparser =
    JD.succeed Todo
        |> JDP.required "id" JD.int
        |> JDP.required "title" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "completion_status" JD.bool


dataparser : JD.Decoder Data
dataparser =
    JD.succeed Data
        |> JDP.required "data" (JD.list todoparser)


loadTodo : Cmd Msg
loadTodo =
    Http.get
        { url = "http://localhost:4000/api/todos"
        , expect = Http.expectJson Gottodos dataparser
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Gettodos ->
            ( Loading, loadTodo )

        Gottodos result ->
            case result of
                Err _ ->
                    let
                        _ =
                            Debug.log "check" result
                    in
                    ( TodoLoadFailed, Cmd.none )

                Ok data ->
                    ( TodoLoaded data, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div [ class "container" ]
        [ header []
            [ div [ class "header-icon" ] [ span [ class "mdi mdi-calendar-check" ] [] ]
            , div [ class "todo-content" ]
                [ span [ class "todo-header" ] [ text "TODO" ]
                , span [ class "todo-description" ] [ text "Track Activities" ]
                ]
            ]
        , section [ class "section-part" ]
            [ div []
                [ button
                    [ class "add-todo-button" ]
                    [ text "ADD TODO" ]
                , button [ class "add-todo-button", onClick Gettodos ] [ text "SHOW EXISTING TODO'S " ]
                ]
            , div [] [ todoitems model ]
            , footer [] []
            ]
        ]



-- div [ class "add-todo" ]
--     [ div [ class "title-section" ]
--         [ span [] [ text "Title" ]
--         , input [ type_ "text", onInput UpdateTodo, value model.todo, onMouseEnter ClearInput ] []
--         ]
--     , div [ class "descriptio-section" ]
--         [ span [] [ text "Description" ]
--         , textarea [ value model.todoDescription ] []
--         ]
--     , div [ class "functionality-section" ]
--         [ button [ onClick AddTodo, class "add-todo-button add-btn" ] [ text "ADD" ]
--         , button [ onClick CancelTodo, class "add-todo-button cancel-btn" ] [ text "Cancel" ]
--         ]
--     ]


todoitems : Model -> Html Msg
todoitems model =
    case model of
        Ready ->
            div [] []

        Loading ->
            div [] [ text "Loading.." ]

        TodoLoaded todos ->
            dataitemsrender todos.data

        TodoLoadFailed ->
            div [] [ text "Failed" ]



-- dataitemsrender : List Todo -> Html.Html msg


dataitemsrender : List { a | title : String, description : String, completion_status : Bool } -> Html msg
dataitemsrender lst =
    div []
        (List.map
            (\element ->
                div [ class "list-container" ]
                    [ div [ class "title-section-list" ]
                        [ text
                            ("   Title :"
                                ++ element.title
                                ++ "   ||       "
                                ++ " Description :"
                                ++ element.description
                                ++ "    ||        "
                                ++ "Status : "
                                ++ (if element.completion_status == True then
                                        "Completed"

                                    else
                                        "Un-Completed"
                                   )
                            )
                        ]
                    , button [ class "mdi mdi-delete-empty delete-section" ] []

                    -- onClick (DeleteTodo todo.id)
                    ]
            )
            lst
        )



-- text element.title
-- newFunction : Todos -> List (Html.Html msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
