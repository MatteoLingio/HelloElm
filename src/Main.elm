module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Debug exposing(log)
import Html.Attributes exposing (type_, checked, placeholder, value, class, classList)

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

type Msg = Add | ChangeTaskText String | ChangeCheck String

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        Add ->
           if model.task.text /= "" then
            { task = { text = "", checked = False }
            , todoList = model.todoList ++ [model.task] }
           else
             model 
        ChangeTaskText text ->
           { model | task = { text = text, checked = False } } 
        ChangeCheck value ->
           { model | todoList = List.map (\n -> checkList n value) model.todoList }
checkList: Task -> String -> Task

checkList element val = 
    if element.text == val && element.checked == True then
        { element | checked = False }
    else if element.text == val && element.checked == False then
        { element | checked = True }
    else
        element

createList: Task -> Html Msg
createList task = 
    li[][ div[class "task-container"][
        input[class "switch", type_ "checkbox", value task.text, onChange ChangeCheck ][]
        ,span[classList [
            ( "checked", task.checked )
          ]][text task.text]] ]

view : Model -> Html Msg

view model =
  div [class "container"][ 
      input[ type_ "text", onInput ChangeTaskText, value model.task.text, placeholder "Add a new task" ][]
      ,div[][]
      ,button [ onClick Add ] [ text "Add Task" ]
      ,div[][]
      ,ul[]
        (List.map createList model.todoList)
      ]


--Styles 
