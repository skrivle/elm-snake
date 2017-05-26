module Actions exposing (Msg(..))

import Time exposing (Time)


type Msg
    = Tick Time
    | Key Int
    | NewFood ( Int, Int )
    | StartPlaying
