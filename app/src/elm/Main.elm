port module Main exposing (main)

import Browser
import Dict exposing ( Dict )
import Maybe exposing ( Maybe )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Json.Decode as D

port setupReceiver : (String -> msg) -> Sub msg
port teardownReceiver : (String -> msg) -> Sub msg

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( { setup = "", teardown = "", graph = Dict.empty }, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.batch [ setupReceiver EditSetup, teardownReceiver EditTeardown ]
    }

type alias Model =
  { setup : String
  , teardown : String
  , graph : Dict Int Node
  }

type alias Graph =
  { nodes : Dict String String
  , edges : Dict String (List String)
  }

type alias Node =
  { name : String
  , expr : String
  }

type Msg =
    NewNode
  | EditNodeName Int String
  | EditNodeExpr Int String
  | EditSetup String
  | EditTeardown String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewNode ->
      ( { model | graph = Dict.insert ( Dict.size model.graph ) { name = "", expr = "" } model.graph }
      , Cmd.none
      )

    EditNodeName i s ->
      ( { model | graph = Dict.update i ( Maybe.map (\n -> {n | name = s}) ) model.graph }
      , Cmd.none
      )

    EditNodeExpr i s ->
      ( { model | graph = Dict.update i ( Maybe.map (\n -> {n | expr = s}) ) model.graph }
      , Cmd.none
      )

    EditSetup    s -> ( { model | setup    = s }, Cmd.none )
    EditTeardown s -> ( { model | teardown = s }, Cmd.none )

view : Model -> Html Msg
view model =
  div [ class "app", onClick NewNode ]
    [ div []
        [ text ( "num nodes: " ++ ( String.fromInt ( Dict.size model.graph ) ) )
        ]
    , div [] ( List.map viewNode ( Dict.toList ( model.graph ) ) )
    ]

viewNode : ( Int, Node ) -> Html Msg
viewNode (i, node) =
  p []
    [ text "node:"
    , input [ value node.name, onInput ( EditNodeName i ) ] []
    , input [ value node.expr ] []
    ]
