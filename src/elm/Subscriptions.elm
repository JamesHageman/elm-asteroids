module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Time
import Keyboard


fps : Float
fps =
    60


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.millisecond * 1000 / fps) Tick
        , Keyboard.downs Keydown
        , Keyboard.ups Keyup
        ]
