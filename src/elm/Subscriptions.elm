module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Keyboard
import AnimationFrame


fps : Float
fps =
    60


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs Keydown
        , Keyboard.ups Keyup
        ]
