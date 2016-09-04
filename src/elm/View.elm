module View exposing (view)

import Model exposing (Model, Msg, Dimensions)
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
                , player model
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


player : Model -> Form
player { ship } =
    polygon
        ship.vertices
        |> filled Color.white
        |> move ship.pos
        |> rotate (degrees ship.angle)


hud : Model -> Form
hud model =
    if model.paused then
        Text.fromString "Paused. Press p to resume"
            |> Text.monospace
            |> Collage.outlinedText (Collage.solid Color.white)
    else
        Collage.group []
