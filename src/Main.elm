import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Matrix
import Array
import Time exposing (Time, second)
import Keyboard
import Random

type Msg = 
  Tick Time 
  | Key Int 
  | NewFood (Int, Int)

type Dir = 
  DOWN 
  | UP 
  | LEFT 
  | RIGHT

type GameItem = 
  Snake 
  | Food

type alias Coord =
  { x: Int
  , y: Int
  }

type alias Food = Coord

type alias SnakePart = Coord

type alias Snake =
  Array.Array SnakePart

type alias Model =
  { dir : Dir
  , food : Food
  , isEating : Bool
  , lockKeys : Bool
  , matrix : Matrix.Matrix (Maybe GameItem)
  , snake : Snake
  }

main : Program Never
main =
    App.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        } 
init : (Model, Cmd a)
init = 
  (model, Cmd.none)

model : Model
model = 
  { snake = 
    [
      { x = 1
      , y = 1
      }
    ] |> Array.fromList
  , matrix = (Matrix.matrix Maybe.Nothing 20 20)
  , dir = DOWN
  , isEating = False
  , lockKeys = False
  , food = 
    { x = 10
    , y = 2
    }
  }

subscriptions : a -> Sub Msg
subscriptions model =
  Sub.batch 
  [ Time.every (second / 6) Tick
  , Keyboard.ups Key
  ]

update : Msg -> Model -> (Model, Cmd Msg)
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
view : Model -> Html Msg
view model =
  let  
    matrix =
      model.matrix
        |> setPointsInMatrix (Array.fromList [model.food]) (Maybe.Just Food)
        |> setPointsInMatrix model.snake (Maybe.Just Snake)
  in
    div [] [
      drawMatrix matrix
    ]


drawMatrix : Matrix.Matrix (Maybe GameItem) -> Html Msg
drawMatrix matrix = 
  table [] 
    (Array.toList (Array.map drawMatrixRow matrix))

drawMatrixRow : Matrix.MatrixRow (Maybe GameItem) -> Html Msg
drawMatrixRow row =
  tr []
    (Array.toList (Array.map drawMatrixCell row))
  
drawMatrixCell : Maybe GameItem -> Html Msg
drawMatrixCell cell =
  let 
    color =
      case cell of
        Maybe.Just item ->
          case item of 
            Snake -> "black"
            Food -> "red"
        Maybe.Nothing ->
          "lightgrey"
  in   
    td [style[
                ("backgroundColor", color), 
                ("height", "20px"), 
                ("width", "20px")
    ]] []

keyCodeToDir : Int -> Maybe Dir
keyCodeToDir keyCode =
  case keyCode of
    40 -> Maybe.Just DOWN
    38 -> Maybe.Just UP
    37 -> Maybe.Just LEFT
    39 -> Maybe.Just RIGHT
    _ -> Maybe.Nothing

setPointsInMatrix : Array.Array Coord -> a -> Matrix.Matrix a -> Matrix.Matrix a 
setPointsInMatrix data value matrix =
  Array.foldr 
    (\coords matrix -> Matrix.set coords.x coords.y value matrix)  
    matrix 
    data

move : Dir -> Snake -> Matrix.Matrix a -> Snake    
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

addPart : Dir -> Snake -> Snake
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

canMove : Dir -> SnakePart -> Snake -> Matrix.Matrix a -> Bool
canMove dir snakePart snake matrix =
  let 
    newPos = calcNextPos dir snakePart
    maxY = (Matrix.height matrix) - 1
    maxX = (Matrix.width matrix) - 1
    notPartOfSnake = 
      snake
        |> Array.filter (\n -> n.x == newPos.x && n.y == newPos.y)
        |> Array.isEmpty
  in
    newPos.x <= maxX  
      && newPos.x >= 0
      && newPos.y <= maxY
      && newPos.y >= 0
      && notPartOfSnake


calcNextPos : Dir -> SnakePart -> SnakePart
calcNextPos dir snakePart =
  case dir of
    DOWN -> 
      {snakePart | y = snakePart.y + 1}
    UP -> 
      {snakePart | y = snakePart.y - 1}
    LEFT -> 
      {snakePart | x = snakePart.x - 1}
    RIGHT -> 
      {snakePart | x = snakePart.x + 1}
    
getOpositeDir : Dir -> Dir
getOpositeDir dir = 
  case dir of 
    LEFT -> RIGHT
    RIGHT -> LEFT
    UP -> DOWN
    DOWN -> UP