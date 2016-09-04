module Model exposing (..)

import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Window exposing (Size)
import Task


type Msg
    = Tick Time
    | Keyup KeyCode
    | Keydown KeyCode
    | TogglePaused
    | Resize Size
    | Noop


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
    , vertices : List Vector2
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
    , thrusting : Bool
    , rotation : Maybe RotateDirection
    , paused : Bool
    , aspectRatio : Float
    , windowSize : Size
    }


init : ( Model, Cmd Msg )
init =
    { ship = initShip
    , asteroids = []
    , dimensions = ( 640, 480 )
    , thrusting = False
    , rotation = Nothing
    , paused = True
    , aspectRatio = 16.0 / 9.0
    , windowSize = { width = 300, height = 300 }
    }
        ! [ Task.perform (always Noop) Resize (Window.size)
          ]


initShip : Ship
initShip =
    { pos = ( 0, 0 )
    , angle = 30
    , velocity = ( 50, 0 )
    , bullets = []
    , vertices =
        [ ( 20, 0 )
        , ( -20, 15 )
        , ( -10, 0 )
        , ( -20, -15 )
        ]
    }
