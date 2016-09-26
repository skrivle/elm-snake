import Html.App as App
import Time exposing (Time, second)
import Keyboard
import View exposing (view)
import Model exposing (Model)
import Actions exposing (Msg(..))
import Update exposing (update)


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
  (Model.initial, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch 
  [ if model.playing then 
      Time.every (second / 6) Tick
    else 
      Sub.none
  , Keyboard.ups Key
  ]