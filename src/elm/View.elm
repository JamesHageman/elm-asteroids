module View exposing (view)

import Model exposing (Model, Msg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Collage exposing (collage, rect, filled, move, rotate, polygon, Form)
import Element exposing (..)
import Color
import Text


view : Model -> Html Msg
view model =
    game model


game : Model -> Html Msg
game model =
    let
        ( width, height ) =
            model.dimensions

        window =
            model.windowSize
    in
        collage
            (round width)
            (round height)
            [ background model
            , player model
            , hud model
            ]
            |> container window.width window.height middle
            |> toHtml


background : Model -> Form
background model =
    let
        ( width, height ) =
            model.dimensions
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
