module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)


---- MODEL ----

type alias Model =
  {
    todos : List Todo,
    newTitle : String,
    newDescription : String
  }
type alias Todo =
  {
    title : String,
    description : String
  }

init : Model
init =
  { 
    todos = [],
    newTitle = "",
    newDescription = ""
  }



---- UPDATE ----

type Msg
  = AddTodo
  | UpdateTodoTitle String
  | UpdateTodoDescription String

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo -> addNewTodo model 
    UpdateTodoTitle title -> 
      { 
        model | newTitle = title
      }
    UpdateTodoDescription description->
      {
        model | newDescription = description
      }

addNewTodo : Model -> Model
addNewTodo model = 
  let
    title = String.trim model.newTitle
    description = String.trim model.newDescription
  in
  case title of 
    "" -> model
    _ -> 
      {
        model | todos = model.todos ++ [{title = title, description = description}],
        newTitle = "",
        newDescription = ""
      }



---- VIEW ----

view : Model -> Html Msg
view model =
  div [] [ 
    viewTable model.todos,
    form [ class "new todo", onSubmit AddTodo] [
      input [
        type_ "text", placeholder "Add new Todo title", value model.newTitle, onInput UpdateTodoTitle
      ] [],
      input [
        type_ "text", placeholder "Add new Todo description", value model.newDescription, onInput UpdateTodoDescription
      ] [],
      button [ disabled (String.isEmpty model.newTitle)] [text "Add"]
    ]
  ]

viewTable : List Todo -> Html Msg
viewTable todos = 
  table [] [
    thead [] [
      th [] [text "Title"],
      th [] [text "Description"]
    ],
    tbody [] (List.map viewTr todos)
  ]

viewTr : Todo -> Html Msg
viewTr todo = 
  tr [] [
    td [] [text todo.title],
    td [] [text todo.description]
  ]


---- PROGRAM ----

main : Program () Model Msg
main =
  Browser.sandbox
    { 
      view = view, 
      init =  init, 
      update = update
    }
