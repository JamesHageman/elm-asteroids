module Model exposing (..)

import Time exposing (Time)
import Keyboard exposing (KeyCode)


type Msg
    = Tick Time
    | Keyup KeyCode
    | Keydown KeyCode
    | TogglePaused


type alias Vector2 =
    ( Float, Float )


type alias Pos =
    Vector2


type alias Dimensions =
    Vector2


type alias Bullet =
    { velocity : Vector2
    , pos : Pos
    }


type alias Ship =
    { pos : Pos
    , angle : Float
    , velocity : Vector2
    , bullets : List Bullet
    }


type alias Asteroid =
    { pos : Pos
    , points : List Pos
    }


type RotateDirection
    = Clockwise
    | Counterclockwise


type alias Model =
    { ship : Ship
    , asteroids : List Asteroid
    , dimensions : Dimensions
    , lastTime : Float
    , thrusting : Bool
    , rotation : Maybe RotateDirection
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    { ship = initShip
    , asteroids = []
    , dimensions = ( 640, 480 )
    , lastTime = 0
    , thrusting = False
    , rotation = Nothing
    , paused = True
    }
        ! []


initShip : Ship
initShip =
    { pos = ( 0, 0 )
    , angle = 30
    , velocity = ( 50, 0 )
    , bullets = []
    }