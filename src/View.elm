module View exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import String
import Array
import Matrix
import Model exposing (Model, GameItem(..), Coord, Food)
import Actions exposing (Msg(..))


view : Model -> Html Msg
view model =
  let  
    matrix =
      Matrix.matrix Maybe.Nothing model.dimensions.width model.dimensions.height
        |> setPointsInMatrix (Array.fromList [model.food]) (Maybe.Just Food)
        |> setPointsInMatrix model.snake (Maybe.Just Snake)
    score = 
      String.append "Score:" (toString model.score)
  in
    if model.playing then 
      div [] 
        [
        drawMatrix matrix
        , div [] [
          text score 
          ]
        ]
    else 
      button [onClick StartPlaying] [
        text "Start playing"
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


setPointsInMatrix : Array.Array Coord -> a -> Matrix.Matrix a -> Matrix.Matrix a 
setPointsInMatrix data value matrix =
  Array.foldr 
    (\coords matrix -> Matrix.set coords.x coords.y value matrix)  
    matrix 
    data