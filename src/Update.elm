module Update exposing (update)

import Random
import Snake exposing (Dir(..))
import Model exposing (Model)
import Actions exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        StartPlaying -> 
          ({model | playing = True}, Cmd.none)
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
            head = 
              Snake.head model.snake
            isEating =
              head.x == model.food.x && head.y == model.food.y
            maxX =
                model.dimensions.width - 1
            maxY = 
                model.dimensions.height - 1
            cmd =
              if isEating == True then
                Random.generate NewFood (Random.pair (Random.int 0 maxX) (Random.int 0 maxY))
              else
                Cmd.none
            snake = 
              let 
                snake =
                  if model.isEating == True then
                    Snake.add model.dir model.snake
                  else
                    model.snake
                in
                  Snake.move model.dir snake model.dimensions
            newScore =
              if model.isEating == True then 
                model.score + 1
              else
                model.score
          in
            case snake of
              Maybe.Just snake ->
                ({model | snake = snake, isEating = isEating, lockKeys = False, score = newScore}, cmd)
              Maybe.Nothing ->
                (Model.initial, cmd)

        Key keyCode -> 
            let 
              dir =
                keyCodeToDir keyCode
            in
              case dir of
                Maybe.Just dir ->
                  if (Snake.opositeDir model.dir) == dir then
                    (model, Cmd.none)
                  else   
                    if model.lockKeys == True then
                      (model, Cmd.none)
                    else 
                      ({model | dir = dir, lockKeys = True}, Cmd.none)
                Maybe.Nothing -> 
                  (model, Cmd.none)


keyCodeToDir : Int -> Maybe Dir
keyCodeToDir keyCode =
  case keyCode of
    40 -> Maybe.Just Down
    38 -> Maybe.Just Up
    37 -> Maybe.Just Left
    39 -> Maybe.Just Right
    _ -> Maybe.Nothing