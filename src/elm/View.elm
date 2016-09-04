module View exposing (view)

import Model exposing (Model, Msg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Collage exposing (collage, rect, filled, move, rotate, polygon, Form)
import Element exposing (..)
import Color


view : Model -> Html Msg
view model =
    div []
        [ game model
        , hud model
        ]


game : Model -> Html Msg
game model =
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
        ship.vertices
        |> filled Color.white
        |> move ship.pos
        |> rotate (degrees ship.angle)


hud : Model -> Html Msg
hud model =
    let
        children =
            if model.paused then
                [ text "Paused. Press p to resume" ]
            else
                []
    in
        div
            [ style
                [ ( "color", "white" )
                , ( "position", "absolute" )
                , ( "top", "0" )
                ]
            ]
            children
