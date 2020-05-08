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
    { allTasks : List Task
     ,toDoList : List Task
     ,doneList : List Task
     ,task : Task
     ,viewSelected : String 
    }

init : Model
init = { allTasks = []
        ,toDoList = []
        ,doneList = []
        ,task = { text = ""
                 , checked = False }
        ,viewSelected = "All"
       }

type Msg = Add | ChangeTaskText String | ChangeCheck String | ChangeView String

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        Add ->
           if model.task.text /= "" then
            { model | task = { text = "", checked = False }
            , allTasks = model.allTasks ++ [model.task]
            , toDoList = model.toDoList ++ [model.task]}
           else
             model 
        ChangeTaskText text ->
           { model | task = { text = text, checked = False } } 
        ChangeCheck value ->
           updateAll value model |> updateLists
        ChangeView current -> 
            { model | viewSelected = current }

updateAll: String -> Model -> Model

updateAll val model =
   { model | allTasks = List.map (\n -> checkList n val) model.allTasks }

updateLists: Model -> Model
updateLists oldModel = 
    { oldModel |  doneList = List.filter(\n -> n.checked == True) oldModel.allTasks
                 ,toDoList = List.filter(\n -> n.checked == False) oldModel.allTasks }

checkList: Task -> String -> Task

checkList element val = 
    if element.text == val && element.checked == True then
        { element | checked = False }
    else if element.text == val && element.checked == False then
        { element | checked = True }
    else
        element

toString : Bool -> String
toString bool =
    if bool then
        "True"

    else
        "False"

taskList: Task -> Html Msg
taskList task = 
    li[][ div[class "task-container"][
        input[class "switch", type_ "checkbox", value task.text, onChange ChangeCheck ][]
        ,span[classList [
            ( "checked", task.checked )
          ]][text task.text]
        ,span[][text (toString task.checked)]] ]

view : Model -> Html Msg

view model =
  div [class "container"][ 
      input[ type_ "text", onInput ChangeTaskText, value model.task.text, placeholder "Add a new task" ][]
      ,div[][]
      ,button [ onClick Add ] [ text "Add Task" ]
      ,div[][]
      ,ul[]
        (if model.viewSelected == "All" then
            List.map taskList model.allTasks
        else if model.viewSelected == "ToDo" then
            List.map taskList model.toDoList
        else
            List.map taskList model.doneList)
      ,(if List.length model.allTasks > 0 then
          div[class "footer"][
          span[onClick (ChangeView "All"), classList [("footer-item", True), ("footer-item_active", model.viewSelected == "All")]][text "All"],
          span[onClick (ChangeView "ToDo"), classList [("footer-item", True), ("footer-item_active", model.viewSelected == "ToDo")]][text "ToDo"],
          span[onClick (ChangeView "Done"), classList [("footer-item", True), ("footer-item_active", model.viewSelected == "Done")]][text "Done"]
        ] else 
            div[][])
      ]


--Styles 
