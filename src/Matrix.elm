module Matrix exposing (matrix, Matrix, set, width, height)

import Array exposing (Array)

type alias Matrix a = Array (MatrixRow a)
type alias MatrixRow a = Array a


matrix: a -> Int -> Int -> Matrix a
matrix value w h = 
    Array.repeat h 
        (Array.repeat w value)

set: Int -> Int -> a -> Matrix a -> Matrix a
set x y value matrix =
    let 

        mapCell index cell =
            if index == x then
                value
            else 
                cell

        mapRows index row =
            if index == y then
                Array.indexedMap mapCell row 
            else 
                row

    in 
        Array.indexedMap mapRows matrix  

indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b  
indexedMap cellFn matrix =
    let 
        mapCell y x cell =
            cellFn x y cell

        mapRows y row =
            Array.indexedMap (mapCell y) row
    in
        Array.indexedMap mapRows matrix 

map: (a -> b) -> Matrix a -> Matrix b
map cellFn matrix =
    let 
        cellFnWrapper x y cell =
            cellFn cell
    in
        indexedMap cellFnWrapper matrix 


width matrix =
    Maybe.withDefault (Array.fromList []) (Array.get 0 matrix)
        |> Array.length

height matrix = 
    Array.length matrix