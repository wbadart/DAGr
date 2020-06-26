port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events as E
import Dict exposing ( Dict )
import Maybe exposing ( Maybe )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing ( Svg, svg )
import Svg.Attributes as A

import Task
import Json.Decode as D

-- ==========
-- Page setup
-- ==========

port setupReceiver : ( String -> msg ) -> Sub msg
port teardownReceiver : ( String -> msg ) -> Sub msg

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( init, Task.attempt FoundSvg ( Browser.Dom.getElement "svg" ) )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Model
init =
  { mousePosition = ( 0, 0 )
  , svgOffset = 0
  , program =
    { setup = ""
    , teardown = ""
    , graph =
        { nodes = Dict.empty
        , edges = Dict.empty
        }
    }
  }

subscriptions : model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ setupReceiver EditSetup
    , teardownReceiver EditTeardown
    , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
    , E.onResize ( \_ _ -> Resized )
    ]


-- ==========
-- Application model
-- ==========


type alias Model =
  { mousePosition : ( Float, Float )
  , svgOffset : Float
  , program :
    { setup : String
    , teardown : String
    , graph : Graph
    }
  }

type alias Graph =
  { nodes : Dict Int Node
  , edges : Dict Int ( List Int )
  }

type alias Node =
  { name : String
  , expr : String
  , pos : ( Float, Float )
  }


type Msg =
    NewNode
  | EditNodeName Int String
  | EditNodeExpr Int String

  | EditSetup String
  | EditTeardown String

  | MouseMove Float Float
  | FoundSvg ( Result Browser.Dom.Error Browser.Dom.Element )
  | Resized


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let prog = model.program
  in
  case msg of
    NewNode ->
      let updatedNodes =
            Dict.insert
              ( Dict.size prog.graph.nodes )
              { name = "", expr = "", pos = model.mousePosition }
              prog.graph.nodes
          g = prog.graph
      in
      ( { model | program = { prog | graph = { g | nodes = updatedNodes } } }
      , Cmd.none
      )

    EditNodeName i s ->
      let updatedNodes = Dict.update i ( Maybe.map (\n -> { n | name = s }) ) prog.graph.nodes
          g = prog.graph
      in
      ( { model | program = { prog | graph = { g | nodes = updatedNodes } } }
      , Cmd.none
      )

    EditNodeExpr i s ->
      let updatedNodes = Dict.update i ( Maybe.map (\n -> { n | expr = s }) ) prog.graph.nodes
          g = prog.graph
      in
      ( { model | program = { prog | graph = { g | nodes = updatedNodes } } }
      , Cmd.none
      )

    EditSetup    s -> ( { model | program = { prog | setup    = s } }, Cmd.none )
    EditTeardown s -> ( { model | program = { prog | teardown = s } }, Cmd.none )

    MouseMove x y -> ( { model | mousePosition = ( x, y ) }, Cmd.none )

    FoundSvg res ->
      case res of
        Err _   -> ( model, Cmd.none )
        Ok elem -> ( { model | svgOffset = elem.element.x }, Cmd.none )

    Resized -> ( model, Task.attempt FoundSvg ( Browser.Dom.getElement "svg" ) )


-- ==========
-- Rendering
-- ==========


view : Model -> Html Msg
view model =
  svg
    [ id "svg", A.width "100%", A.height "100%", onClick NewNode ]
    ( List.map ( viewNode model.svgOffset ) ( Dict.toList model.program.graph.nodes ) )

viewNode : Float -> ( Int, Node ) -> Svg Msg
viewNode xOffset ( i, node ) =
  let ( x, y ) = node.pos
  in
  Svg.rect
    [ A.x ( ( String.fromFloat ( x - xOffset ) ) ++ "px" )
    , A.y ( ( String.fromFloat ( y ) ) ++ "px" )
    , A.width "100"
    , A.height "100"
    ]
    [ Svg.text node.name
    ]
