module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Debug exposing(log)
import Html.Attributes exposing (type_, checked, value, class)

main = Browser.sandbox {init = init, update = update, view = view}

--Model
type alias Task =
  { text : String
  , checked : Bool
  }
type alias Model = 
    { todoList : List Task
     ,task : Task 
    }

init : Model
init = { todoList = []
        ,task = { text = ""
                 , checked = False }  
       }

type Msg = Add | ChangeTaskText String

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        Add ->
           { task = { text = "", checked = False }
           , todoList = model.todoList ++ [model.task] }
        ChangeTaskText text ->
           { model | task = { text = text, checked = False } } 



createList: Task -> Html Msg
createList task = 
    li[][ div[class "task-container"][
        input[class "switch", type_ "checkbox" ][]
        ,text task.text] ]

view : Model -> Html Msg

view model =
  div [class "container"][ 
      input[ type_ "text", onInput ChangeTaskText ][]
      ,div[][]
      ,button [ onClick Add ] [ text "Add Task" ]
      ,div[][]
      ,ul[]
        (List.map createList model.todoList)
      ]


--Styles 
