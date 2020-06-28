port module Main exposing ( main )

import Browser
import Dict exposing ( Dict )
import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( .. )
import Http
import Json.Decode as D
import Json.Encode as E
import Maybe exposing ( Maybe )

import Debug exposing ( .. )


-- ==========
-- Page setup
-- ==========


port setupReceiver    : ( String  -> msg ) -> Sub msg
port teardownReceiver : ( String  -> msg ) -> Sub msg
port graphReceiver    : ( E.Value -> msg ) -> Sub msg
port newNode          :   E.Value          -> Cmd msg
port compiledProgram  :   String           -> Cmd msg

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( init , Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Model
init =
  { setup    = ""
  , teardown = ""
  , graph =
      { nodes = Dict.empty
      , edges = Dict.empty
      }
  , newExpr = ""
  }

subscriptions : model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ setupReceiver    EditSetup
    , teardownReceiver EditTeardown
    , graphReceiver    EditGraph
    ]


-- ==========
-- Application model
-- ==========


type alias Model =
  { setup    : String
  , teardown : String
  , graph    : Graph
  , newExpr  : String
  }

type alias Graph =
  { nodes : Dict String String
  , edges : Dict String ( List String )
  }

type Msg =
    EditSetup    String
  | EditTeardown String
  | EditGraph    E.Value

  | EditNodeExpr String
  | Submit
  | GetCompiledProgram
  | GotCompiledProgram ( Result Http.Error String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    EditSetup    s -> ( { model |  setup    = s }, Cmd.none )
    EditTeardown s -> ( { model |  teardown = s }, Cmd.none )

    EditGraph obj ->
      case D.decodeValue decodeGraph obj of
        Err _ -> ( model, Cmd.none )
        Ok  a -> ( { model | graph = parseGraph a }, Cmd.none )

    EditNodeExpr s -> ( { model | newExpr = s }, Cmd.none )
    Submit ->
      ( { model | newExpr = "" }
      , newNode ( E.string model.newExpr )
      )
    GetCompiledProgram ->
      ( model
      , Http.post
          { url = "http://localhost:3000/"
          , body = Http.jsonBody <| encodeProrgam model
          , expect = Http.expectString GotCompiledProgram
          }
      )
    GotCompiledProgram s ->
      case s of
        Err _ -> ( model, Cmd.none )
        Ok prog -> ( model, compiledProgram prog )


encodeProrgam : Model -> E.Value
encodeProrgam model =
  E.object
    [ ( "setup", E.string model.setup )
    , ( "teardown", E.string model.teardown )
    , ( "graph"
      , E.object
          [ ( "nodes"
            , E.dict identity E.string model.graph.nodes
            )
          , ( "edges"
            , E.dict
                identity
                ( E.list E.string )
                model.graph.edges
            )
          ]
      )
    ]

type alias IntermediateGraph =
  { nodes : Dict String String
  , edges : List ( Int, ( String, String ) )
  }
decodeGraph : D.Decoder IntermediateGraph
decodeGraph =
  D.map2
    IntermediateGraph
    ( D.field "nodes" <| D.dict D.string )
    ( D.field "edges"
        <| D.list
        <| D.map2
             Tuple.pair
             ( D.field "order" D.int )
             <| D.map2
                  Tuple.pair
                    ( D.field "src" D.string )
                    ( D.field "dst" D.string )
    )

parseGraph : IntermediateGraph -> Graph
parseGraph d =
  { nodes = d.nodes
  , edges =
      d.edges
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> log "foo"
        |> List.foldr addLink Dict.empty
  }

addLink : ( String, String ) -> Dict String ( List String ) -> Dict String ( List String )
addLink ( src, dst ) acc = Dict.update dst ( appendSource src ) acc

appendSource : String -> Maybe ( List String ) -> Maybe ( List String )
appendSource new orig =
  case orig of
    Just srcs -> Just ( new :: srcs )
    Nothing -> Just [ new ]


-- ==========
-- Rendering
-- ==========


view : Model -> Html Msg
view model =
  div []
    [ input
      [ type_ "text"
      , placeholder "expression"
      , value model.newExpr
      , onInput EditNodeExpr
      ]
      []
    , p []
        [ button [ onClick Submit ] [ text "Add Node" ]
        , button [ onClick GetCompiledProgram ] [ text "Compile" ]
        ]
    ]
