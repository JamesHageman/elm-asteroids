module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Keyboard
import AnimationFrame
import Window


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs Keydown
        , Keyboard.ups Keyup
        , Window.resizes Resize
        ]
