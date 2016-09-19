module Update exposing (update)

import Model exposing (Model, Pos, Dimensions, Msg(..), RotateDirection(..), Asteroid, Ship, Bullet)
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import Task
import Window exposing (Size)
import PageVisibility exposing (Visibility(Visible, Hidden))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            updateModel msg model

        fx =
            fireEffects msg model
    in
        ( newModel, fx )


fireAction : msg -> Cmd msg
fireAction msg =
    Task.perform identity identity (Task.succeed msg)


fireEffects : Msg -> Model -> Cmd Msg
fireEffects msg model =
    case msg of
        Keydown code ->
            if code == keyMap.pause then
                fireAction TogglePaused
            else if code == keyMap.shoot then
                if not model.paused then
                    fireAction Shoot
                else
                    Cmd.none
            else
                Cmd.none

        _ ->
            Cmd.none


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Tick delta ->
            if model.paused then
                model
            else
                model
                    |> moveAsteroids delta
                    |> moveBullets delta
                    |> rotateShip delta
                    |> accelerateShip delta
                    |> moveShip delta

        Shoot ->
            model
                |> fireBullet

        Keyup code ->
            trackKeyUp code model

        Keydown code ->
            trackKeyDown code model

        TogglePaused ->
            { model | paused = not model.paused }

        Resize size ->
            model
                |> updateWindowSize size
                |> updateScaleFactor size

        PageVisibilityChanged visibility ->
            case visibility of
                Hidden ->
                    { model | paused = True }

                Visible ->
                    model

        Noop ->
            model


updateWindowSize : Size -> Model -> Model
updateWindowSize size model =
    { model | windowSize = size }


updateScaleFactor : Size -> Model -> Model
updateScaleFactor size model =
    let
        { worldDimensions, aspectRatio } =
            model
    in
        { model | scaleFactor = getScaleFactor size worldDimensions aspectRatio }


fireBullet : Model -> Model
fireBullet model =
    let
        { ship } =
            model

        newShip =
            { ship
                | bullets =
                    newBullet ship :: ship.bullets
            }
    in
        { model | ship = newShip }


newBullet : Ship -> Bullet
newBullet ship =
    let
        speed =
            200

        ( shipVx, shipVy ) =
            ship.velocity

        vx =
            speed * cos (degrees ship.angle)

        vy =
            speed * sin (degrees ship.angle)
    in
        { velocity = ( vx + shipVx, vy + shipVy )
        , pos = ship.pos
        }


moveBullets : Time -> Model -> Model
moveBullets delta model =
    let
        { ship } =
            model

        { bullets } =
            ship

        newBullets =
            bullets
                |> List.map (moveBullet delta model.worldDimensions)

        newShip =
            { ship
                | bullets = newBullets
            }
    in
        { model | ship = newShip }


moveBullet : Time -> Dimensions -> Bullet -> Bullet
moveBullet delta worldDimensions bullet =
    let
        ( vx, vy ) =
            bullet.velocity

        dt =
            Time.inSeconds delta

        ( x, y ) =
            bullet.pos

        newPos =
            ( x + vx * dt, y + vy * dt )
                |> wrap worldDimensions
    in
        { bullet | pos = newPos }


moveAsteroids : Time -> Model -> Model
moveAsteroids delta model =
    let
        newAsteroids =
            model.asteroids
                |> List.map (moveAsteroid delta model.worldDimensions)
    in
        { model | asteroids = newAsteroids }


moveAsteroid : Time -> Dimensions -> Asteroid -> Asteroid
moveAsteroid delta dimensions asteroid =
    let
        { direction } =
            asteroid

        theta =
            degrees direction

        ( x, y ) =
            asteroid.pos

        speed =
            50 * (Time.inSeconds delta)

        newPos =
            ( x + speed * (cos theta)
            , y + speed * (sin theta)
            )
                |> wrap dimensions
    in
        { asteroid | pos = newPos }


getScaleFactor : Window.Size -> Dimensions -> Float -> Float
getScaleFactor windowSize ( worldWidth, worldHeight ) aspectRatio =
    let
        windowRatio =
            (toFloat windowSize.height) / (toFloat windowSize.width)
    in
        if windowRatio > aspectRatio then
            (toFloat windowSize.width) / worldWidth
        else
            (toFloat windowSize.height) / worldHeight


rotateShip : Time -> Model -> Model
rotateShip delta model =
    let
        deltaSeconds =
            Time.inSeconds delta

        rotation =
            360 * deltaSeconds

        { ship } =
            model

        newShip =
            case model.rotation of
                Nothing ->
                    ship

                Just Counterclockwise ->
                    { ship | angle = ship.angle + rotation }

                Just Clockwise ->
                    { ship | angle = ship.angle - rotation }
    in
        { model | ship = newShip }


accelerateShip : Time -> Model -> Model
accelerateShip delta model =
    let
        accel =
            200

        { ship } =
            model

        ( vx, vy ) =
            model.ship.velocity

        ax =
            accel * cos (degrees ship.angle)

        ay =
            accel * sin (degrees ship.angle)

        dt =
            Time.inSeconds delta

        newVelocity =
            if model.thrusting then
                ( vx + ax * dt, vy + ay * dt )
            else
                model.ship.velocity

        newShip =
            { ship | velocity = newVelocity }
    in
        { model | ship = newShip }


moveShip : Time -> Model -> Model
moveShip delta model =
    let
        { ship, thrusting } =
            model

        ( x, y ) =
            ship.pos

        ( vx, vy ) =
            ship.velocity

        deltaSeconds =
            Time.inSeconds delta

        newPos =
            ( x + vx * deltaSeconds, y + vy * deltaSeconds )
                |> wrap model.worldDimensions

        newShip =
            { ship | pos = newPos }
    in
        { model | ship = newShip }


wrap : Dimensions -> Pos -> Pos
wrap ( width, height ) ( x, y ) =
    let
        newX =
            if x > width / 2 then
                x - width
            else if x < -width / 2 then
                x + width
            else
                x

        newY =
            if y > height / 2 then
                y - height
            else if y < -height / 2 then
                y + height
            else
                y
    in
        ( newX, newY )


keyMap =
    { thrust = 38
    , rotateLeft = 37
    , rotateRight = 39
    , pause = 80
    , shoot = 32
    }


trackKeyUp : KeyCode -> Model -> Model
trackKeyUp code model =
    { model
        | thrusting =
            if code == keyMap.thrust then
                False
            else
                model.thrusting
        , rotation =
            if code == keyMap.rotateLeft || code == keyMap.rotateRight then
                Nothing
            else
                model.rotation
    }


trackKeyDown : KeyCode -> Model -> Model
trackKeyDown code model =
    let
        _ =
            Debug.log "code" code
    in
        { model
            | thrusting =
                if code == keyMap.thrust then
                    True
                else
                    model.thrusting
            , rotation =
                if code == keyMap.rotateLeft then
                    Just Counterclockwise
                else if code == keyMap.rotateRight then
                    Just Clockwise
                else
                    model.rotation
        }
