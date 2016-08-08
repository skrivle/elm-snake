import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Matrix
import Array
import Time exposing (Time, second)
import Keyboard
import Random

type Msg = Tick Time | Key Int | NewFood (Int, Int)

type DIR = DOWN | UP | LEFT | RIGHT

type MatrixItem = SNAKE | FOOD

type alias SnakePart = 
  { x : Int
  , y : Int
  }

keyCodeToDir keyCode =
  case keyCode of
    40 -> Maybe.Just DOWN
    38 -> Maybe.Just UP
    37 -> Maybe.Just LEFT
    39 -> Maybe.Just RIGHT
    _ -> Maybe.Nothing

myMatrix =
    (Matrix.matrix Maybe.Nothing 20 20)

mySnake = 
  [
    { x = 1
    , y = 1
    }
  ]
  |> Array.fromList

model = 
  {
    snake = mySnake,
    matrix = myMatrix,
    dir = DOWN,
    isEating = False,
    lockKeys = False,
    food = 
      { x = 10
      , y = 2
      }
  }

init = 
  (model, Cmd.none)

subscriptions model =
  Sub.batch 
  [ Time.every (second / 6) Tick
  , Keyboard.ups Key
  ]
  

setPointsInMatrix data value matrix =
  Array.foldr 
    (\coords matrix -> Matrix.set coords.x coords.y value matrix)  
    matrix 
    data
    
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

addPart dir snake =
    let 
      snakeLength =
        Array.length snake
      lastPart = 
        snake 
          |> Array.get (snakeLength - 1)  
     in
      case lastPart of
        Maybe.Just part ->
          let 
            newPart = 
              part
                |> calcNextPos (getOpositeDir dir)
          in
            snake
              |> Array.push newPart
        Maybe.Nothing -> 
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
    App.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        } 
view model =
  let 
    matrix =
      myMatrix
        |> setPointsInMatrix (Array.fromList [model.food]) (Maybe.Just FOOD)
        |> setPointsInMatrix model.snake (Maybe.Just SNAKE)
  in
    div [] [
      drawMatrix matrix
      -- button [onClick (CHANGE_DIR DOWN)] [text "down"],
      -- button [onClick (CHANGE_DIR UP)] [text "up"],
      -- button [onClick (CHANGE_DIR LEFT)] [text "left"],
      -- button [onClick (CHANGE_DIR RIGHT)] [text "right"]
    ]

getOpositeDir dir = 
  case dir of 
    LEFT -> RIGHT
    RIGHT -> LEFT
    UP -> DOWN
    DOWN -> UP


update msg model =
    case msg of 
        NewFood coords ->
          let 
            newFood = 
              { x = (fst coords)
              , y = (snd coords) 
              }
          in
            ({model | food = newFood}, Cmd.none)
        Tick time ->
          let 
            maxY = 
              (Matrix.height model.matrix) - 1
            maxX = 
              (Matrix.width model.matrix) - 1
            firstPart = 
              Maybe.withDefault {x = -1, y = -1} (Array.get 0 model.snake)
            isEating =
              firstPart.x == model.food.x && firstPart.y == model.food.y
            cmd =
              if isEating == True then
                Random.generate NewFood (Random.pair (Random.int 0 maxX) (Random.int 0 maxY))
              else
                Cmd.none
            snake = 
              if model.isEating == True then
                let 
                  newSnake = 
                    addPart model.dir model.snake
                in
                  move model.dir newSnake model.matrix 
              else 
                move model.dir model.snake model.matrix
          in
            ({model | snake = snake, isEating = isEating, lockKeys = False}, cmd)
        Key keyCode -> 
            let 
              dir =
                keyCodeToDir keyCode
            in
              case dir of
                Maybe.Just dir ->
                  if (getOpositeDir model.dir) == dir then
                    (model, Cmd.none)
                  else   
                    if model.lockKeys == True then
                      (model, Cmd.none)
                    else 
                      ({model | dir = dir, lockKeys = True}, Cmd.none)
                Maybe.Nothing -> 
                  (model, Cmd.none)

drawMatrix matrix = 
  table [] 
    (Array.toList (Array.map drawMatrixRow matrix))
  
drawMatrixRow row =
  tr []
    (Array.toList (Array.map drawMatrixCell row))
  
drawMatrixCell cell =
  let 
    color =
      case cell of
        Maybe.Just item ->
          case item of 
            SNAKE -> "black"
            FOOD -> "red"
        Maybe.Nothing ->
          "lightgrey"
  in   
    td [style[
                ("backgroundColor", color), 
                ("height", "20px"), 
                ("width", "20px")
    ]] []  