module Main exposing (main)

import Browser
import Dict exposing ( Dict )
import Maybe exposing ( Maybe )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Json.Decode as D

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( { nodes = Dict.empty }, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Model =
  { nodes : Dict Int Node
  }

type alias Node =
  { name : String
  , expr : String
  }

type Msg =
    NewNode
  | EditNodeName Int String
  | EditNodeExpr Int String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewNode ->
      ( { model | nodes = Dict.insert ( Dict.size model.nodes ) { name = "", expr = "" } model.nodes }
      , Cmd.none
      )

    EditNodeName i s ->
      ( { model | nodes = Dict.update i ( Maybe.map (\n -> {n | name = s}) ) model.nodes }
      , Cmd.none
      )

    EditNodeExpr i s ->
      ( { model | nodes = Dict.update i ( Maybe.map (\n -> {n | expr = s}) ) model.nodes }
      , Cmd.none
      )


view : Model -> Html Msg
view model =
  div [ class "app", onClick NewNode ]
    [ div []
        [ text ( "num nodes: " ++ ( String.fromInt ( Dict.size model.nodes ) ) )
        ]
    , div [] ( List.map viewNode ( Dict.toList ( model.nodes ) ) )
    ]

viewNode : ( Int, Node ) -> Html Msg
viewNode (i, node) =
  p []
    [ text "node:"
    , input [ value node.name, onInput ( EditNodeName i ) ] []
    , input [ value node.expr ] []
    ]
