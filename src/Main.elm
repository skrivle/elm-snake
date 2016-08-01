import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.App as App
import Matrix
import Array
import Debug

type Msg = DIR

type DIR = DOWN | UP | LEFT | RIGHT

myMatrix =
    (Matrix.matrix 0 20 20)

mySnake = 
  [
    {
      x = 3
      , y = 2
    },
    {
      x = 2
      , y = 2
    },
    {
      x = 2
      , y = 1
    },
    {
      x = 1
      , y = 1
    },
    {
      x = 0
      , y = 1
    },
    {
      x = 0
      , y = 0
    }
  ]
  |> Array.fromList

model = 
  {
    snake = mySnake,
    matrix = myMatrix
  }

setPointsInMatrix data matrix =
  Array.foldr setPointInMatrix matrix data
    
setPointInMatrix coords matrix =
  Matrix.set coords.x coords.y 1 matrix

move dir snake matrix =
  let
    firstPart = Maybe.withDefault  {x = -1, y = -1} (Array.get 0 snake)
    isAbleToMove = canMove dir firstPart snake matrix
    mapper index part = 
      if index == 0 then 
        calcNextPos dir part
      else 
        Maybe.withDefault {x = -1, y = -1} (Array.get (index - 1) snake)
  in 
    if isAbleToMove == True then
      Array.indexedMap mapper snake
    else
      snake

canMove dir part parts matrix =
  let 
    newPos = calcNextPos dir part
    maxY = (Matrix.height matrix) - 1
    maxX = (Matrix.width matrix) - 1
    notPartOfSnake = 
      parts
        |> Array.filter (\n -> n.x == newPos.x && n.y == newPos.y)
        |> Array.isEmpty
  in
    newPos.x <= maxX  
      && newPos.x >= 0
      && newPos.y <= maxY
      && newPos.y >= 0
      && notPartOfSnake




calcNextPos dir part =
  case dir of
    DOWN -> 
      {part | y = part.y + 1}
    UP -> 
      {part | y = part.y - 1}
    LEFT -> 
      {part | x = part.x - 1}
    RIGHT -> 
      {part | x = part.x + 1}
    
main : Program Never
main =
    App.beginnerProgram 
        {
            model = model
            , view = view
            , update = update
        } 
view model =

  div [] [
    drawMatrix (setPointsInMatrix model.snake myMatrix),
    button [onClick DOWN] [text "down"],
    button [onClick UP] [text "up"],
    button [onClick LEFT] [text "left"],
    button [onClick RIGHT] [text "right"]
  ]
  

update msg model =
    case msg of 
        DOWN ->
            {model | snake = move DOWN model.snake model.matrix}
        UP ->
            {model | snake = move UP model.snake model.matrix}
        LEFT ->
            {model | snake = move LEFT model.snake model.matrix}
        RIGHT ->
            {model | snake = move RIGHT model.snake model.matrix}

drawMatrix matrix = 
  table [] 
    (Array.toList (Array.map drawMatrixRow matrix))
  
drawMatrixRow row =
  tr []
    (Array.toList (Array.map drawMatrixCell row))
  
drawMatrixCell cell =
  let 
    color =
      if cell == 1 then
        "black"
      else
        "lightgrey"
  in   
    td [style[
                ("backgroundColor", color), 
                ("height", "20px"), 
                ("width", "20px")
    ]] []  