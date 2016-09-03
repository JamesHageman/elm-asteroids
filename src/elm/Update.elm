module Update exposing (update)

import Model exposing (Model, Pos, Dimensions, Msg(..), RotateDirection(..))
import Time exposing (Time)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            updateModel msg model

        fx =
            fireEffects msg model
    in
        ( newModel, fx )


fireEffects : Msg -> Model -> Cmd Msg
fireEffects msg model =
    case msg of
        Keydown code ->
            if code == keyMap.pause then
                Task.perform identity identity (Task.succeed TogglePaused)
            else
                Cmd.none

        _ ->
            Cmd.none


updateModel : Msg -> Model -> Model
updateModel msg model =
    if model.paused then
        case msg of
            TogglePaused ->
                { model | paused = False, lastTime = 0 }

            _ ->
                model
    else
        case msg of
            Tick time ->
                let
                    delta =
                        calculateDelta time model
                in
                    model
                        |> rotateShip delta
                        |> accelerateShip delta
                        |> moveShip delta
                        |> updateLastTime time

            Keyup code ->
                trackKeyUp code model

            Keydown code ->
                trackKeyDown code model

            TogglePaused ->
                { model | paused = True }


calculateDelta : Time -> Model -> Time
calculateDelta time { lastTime } =
    case lastTime of
        0 ->
            0

        lastTime ->
            (Time.inMilliseconds time) - (Time.inMilliseconds lastTime)


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
                |> wrap model.dimensions

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


updateLastTime : Time -> Model -> Model
updateLastTime time model =
    { model | lastTime = time }


keyMap =
    { thrust = 38
    , rotateLeft = 37
    , rotateRight = 39
    , pause = 80
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