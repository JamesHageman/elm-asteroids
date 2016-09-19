module View exposing (view)

import Model exposing (Model, Msg, Dimensions, Asteroid, AsteroidSize(..), Bullet)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Collage exposing (collage, rect, filled, move, rotate, polygon, Form)
import Element exposing (..)
import Color
import Text
import Window


view : Model -> Html Msg
view model =
    game model
        |> toHtml


game : Model -> Element
game model =
    let
        { windowSize, worldDimensions, aspectRatio } =
            model

        ( width, height ) =
            model.worldDimensions

        gameForm =
            Collage.group
                [ background model
                , asteroids model
                , player model
                , bullets model
                , hud model
                ]
                |> Collage.scale model.scaleFactor
    in
        collage
            (width * model.scaleFactor |> round)
            (height * model.scaleFactor |> round)
            [ gameForm ]
            |> container windowSize.width windowSize.height middle


background : Model -> Form
background model =
    let
        ( width, height ) =
            model.worldDimensions
    in
        rect width height
            |> filled Color.black


bullets : Model -> Form
bullets model =
    let
        { bullets } =
            model.ship
    in
        bullets
            |> List.map renderBullet
            |> Collage.group


renderBullet : Bullet -> Form
renderBullet bullet =
    Collage.circle 5
        |> filled Color.white
        |> move bullet.pos


asteroids : Model -> Form
asteroids model =
    model.asteroids
        |> List.map renderAsteroid
        |> List.concatMap (replicateForWrapping model.worldDimensions)
        |> Collage.group


renderAsteroid : Asteroid -> Form
renderAsteroid asteroid =
    Collage.ngon 12 (asteroidRadius asteroid.size)
        |> Collage.outlined (Collage.solid Color.white)
        |> move asteroid.pos


asteroidRadius : AsteroidSize -> Float
asteroidRadius size =
    case size of
        Small ->
            10

        Medium ->
            25

        Large ->
            50


player : Model -> Form
player { ship, worldDimensions } =
    let
        ( width, height ) =
            worldDimensions

        turn =
            rotate (degrees ship.angle)

        shape =
            polygon
                ship.vertices
                |> filled Color.white
                |> move ship.pos
    in
        shape
            |> replicateForWrapping worldDimensions
            |> List.map turn
            |> Collage.group


replicateForWrapping : Dimensions -> Form -> List Form
replicateForWrapping worldDimensions form =
    let
        ( width, height ) =
            worldDimensions
    in
        [ form
        , form |> move ( width, 0 )
        , form |> move ( -width, 0 )
        , form |> move ( 0, height )
        , form |> move ( 0, -height )
        ]


hud : Model -> Form
hud model =
    if model.paused then
        Text.fromString "Paused. Press p to resume"
            |> Text.monospace
            |> Collage.outlinedText (Collage.solid Color.white)
    else
        Collage.group []
