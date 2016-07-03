import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix
import Array
import Debug

myMatrix =
    (Matrix.matrix 0 20 20)
      |> Matrix.set 0 10 1
      -- |> moveUp
      |> moveDown
     
-- moveDown matrix =
--     let 
--       mapCell x y cell =

--     Matrix.indexMap  

moveDown matrix =
  let 
    step curr memo =
      {memo | 
        result = Array.push memo.lastRow memo.result
        , lastRow = curr
      }
    start = {
      result = Array.fromList []
      , lastRow = Array.repeat (Matrix.width matrix) 0
    }

    result = Array.foldl step start  matrix
  in
    result.result


moveUp matrix =
  let 
    step curr memo =
      {memo | 
        result = Array.push memo.lastRow memo.result
        , lastRow = curr
      }
    start = {
      result = Array.fromList []
      , lastRow = Array.repeat (Matrix.width matrix) 0
    }

    result = Array.foldr step start  matrix
  in
    Array.fromList (List.reverse(Array.toList result.result))
    

main = 
  drawMatrix myMatrix

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