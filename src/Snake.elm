module Snake exposing (Dir(..), SnakePart, Snake, head, move, canMove, add, opositeDir, fromList)

import Array exposing (Array)


type Dir
    = Down
    | Up
    | Left
    | Right


type alias SnakePart =
    { x : Int
    , y : Int
    }


type alias Snake =
    Array SnakePart


type alias Dimensions =
    { width : Int
    , height : Int
    }


move : Dir -> Snake -> Dimensions -> Maybe Snake
move dir snake dimensions =
    let
        isAbleToMove =
            canMove dir (head snake) snake dimensions
    in
        if isAbleToMove == True then
            snake
                |> Array.slice 0 -1
                |> Array.append (Array.fromList [ calcNextPos dir (head snake) ])
                |> Maybe.Just
        else
            Maybe.Nothing


add : Dir -> Snake -> Snake
add dir snake =
    let
        newPart =
            calcNextPos (opositeDir dir) (tail snake)
    in
        Array.push newPart snake


canMove : Dir -> SnakePart -> Snake -> Dimensions -> Bool
canMove dir snakePart snake dimensions =
    let
        maxX =
            dimensions.width - 1

        maxY =
            dimensions.height - 1

        newPos =
            calcNextPos dir snakePart

        notPartOfSnake =
            snake
                |> Array.filter (\n -> n.x == newPos.x && n.y == newPos.y)
                |> Array.isEmpty
    in
        newPos.x
            <= maxX
            && newPos.x
            >= 0
            && newPos.y
            <= maxY
            && newPos.y
            >= 0
            && notPartOfSnake


calcNextPos : Dir -> SnakePart -> SnakePart
calcNextPos dir snakePart =
    case dir of
        Down ->
            { snakePart | y = snakePart.y + 1 }

        Up ->
            { snakePart | y = snakePart.y - 1 }

        Left ->
            { snakePart | x = snakePart.x - 1 }

        Right ->
            { snakePart | x = snakePart.x + 1 }


opositeDir : Dir -> Dir
opositeDir dir =
    case dir of
        Left ->
            Right

        Right ->
            Left

        Up ->
            Down

        Down ->
            Up


head : Snake -> SnakePart
head snake =
    Maybe.withDefault { x = -1, y = -1 } (Array.get 0 snake)


tail : Snake -> SnakePart
tail snake =
    let
        lastIndex =
            (length snake) - 1

        tail =
            Array.get lastIndex snake
    in
        case tail of
            Maybe.Just tail ->
                tail

            Maybe.Nothing ->
                { x = -1
                , y = -1
                }


fromList : List SnakePart -> Snake
fromList list =
    Array.fromList list


length : Snake -> Int
length snake =
    Array.length snake
