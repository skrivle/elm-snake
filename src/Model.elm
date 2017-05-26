module Model exposing (Model, GameItem(..), Coord, Food, initial)

import Matrix exposing (MatrixDimensions)
import Snake exposing (Dir(..), Snake)


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Food =
    Coord


type GameItem
    = Snake
    | Food


type alias Model =
    { dir : Dir
    , food : Food
    , isEating : Bool
    , lockKeys : Bool
    , playing : Bool
    , dimensions : MatrixDimensions
    , snake : Snake.Snake
    , score : Int
    }


initial : Model
initial =
    { snake =
        [ { x = 1
          , y = 1
          }
        ]
            |> Snake.fromList
    , dimensions =
        { width = 20
        , height = 20
        }
    , dir = Down
    , isEating = False
    , lockKeys = False
    , playing = False
    , score = 0
    , food =
        { x = 10
        , y = 2
        }
    }
