module View exposing (view)

import Model exposing (Model, Msg)
import Html exposing (Html, div, text)
import Collage exposing (..)
import Element exposing (..)
import Color


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            model.dimensions
    in
        collage
            (floor width)
            (floor height)
            [ background model
            , player model
            ]
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
        [ ( 20, 0 ), ( -20, 15 ), ( -20, -15 ) ]
        |> filled Color.red
        |> move ship.pos
        |> rotate (degrees ship.angle)
