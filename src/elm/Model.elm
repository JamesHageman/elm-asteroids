module Model exposing (..)

import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Window exposing (Size)
import Task
import PageVisibility exposing (Visibility)


type Msg
    = Tick Time
    | Keyup KeyCode
    | Keydown KeyCode
    | TogglePaused
    | Resize Size
    | PageVisibilityChanged Visibility
    | Shoot
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
    { pos : Vector2
    , size : AsteroidSize
    , direction : Float
    }


type AsteroidSize
    = Small
    | Medium
    | Large


type RotateDirection
    = Clockwise
    | Counterclockwise


type alias Model =
    { ship : Ship
    , asteroids : List Asteroid
    , worldDimensions : Dimensions
    , thrusting : Bool
    , rotation : Maybe RotateDirection
    , paused : Bool
    , aspectRatio : Float
    , windowSize : Size
    , scaleFactor : Float
    }


init : ( Model, Cmd Msg )
init =
    { ship = initShip
    , asteroids =
        [ initAsteroid ( -100, 200 ) -10
        , initAsteroid ( 50, -200 ) 30
        , initAsteroid ( 20, -300 ) 100
        ]
    , worldDimensions = ( 450, 800 )
    , thrusting = False
    , rotation = Nothing
    , paused = True
    , aspectRatio = 16.0 / 9.0
    , windowSize = { width = 300, height = 300 }
    , scaleFactor = 1
    }
        ! [ Task.perform (always Noop) Resize (Window.size)
          ]


initAsteroid : Vector2 -> Float -> Asteroid
initAsteroid pos direction =
    { pos = pos
    , size = Large
    , direction = direction
    }


initShip : Ship
initShip =
    { pos = ( 0, -50 )
    , angle = 0
    , velocity = ( 30, 0 )
    , bullets = []
    , vertices =
        [ ( 20, 0 )
        , ( -20, 15 )
        , ( -10, 0 )
        , ( -20, -15 )
        ]
    }
