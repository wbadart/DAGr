port module Main exposing ( main )

import Browser
import Dict exposing ( Dict )
import Html exposing ( Html, div )
import Html.Attributes exposing ( style )
import Json.Decode as D exposing ( .. )
import Json.Encode as E
import Maybe exposing ( Maybe )


-- ==========
-- Page setup
-- ==========


port setupReceiver    : ( String  -> msg ) -> Sub msg
port teardownReceiver : ( String  -> msg ) -> Sub msg
port graphReceiver    : ( E.Value -> msg ) -> Sub msg

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
  }

type alias Graph =
  { nodes : Dict String String
  , edges : Dict String ( List String )
  }

type Msg =
    EditSetup    String
  | EditTeardown String
  | EditGraph    E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    EditSetup    s -> ( { model |  setup    = s }, Cmd.none )
    EditTeardown s -> ( { model |  teardown = s }, Cmd.none )
    EditGraph obj ->
      case D.decodeValue decodeGraph obj of
        Err _ -> ( model, Cmd.none )
        Ok  a -> ( { model | graph = parseGraph a }, Cmd.none )

decodeGraph : D.Decoder ( List ( Dict String String ) )
decodeGraph = field "cells" ( list ( dict string ) )

parseGraph : List ( Dict String String ) -> Graph
parseGraph d =
  { nodes =
      d
        |> List.filter ( cellType "devs.Model" )
        |> List.map parseNode
        |> Dict.fromList
  , edges =
      d
        |> List.filter ( cellType "devs.Link" )
        |> List.foldl addLink Dict.empty
  }

cellType : String -> Dict String String -> Bool
cellType type_ c =
  case Dict.get "type" c of
    Nothing -> False
    Just t -> t == type_

parseNode : Dict String String -> ( String, String )
parseNode obj = ( "foo", "bar" )

addLink : Dict String String -> Dict String ( List String ) -> Dict String ( List String )
addLink e acc =
  case ( Dict.get "source" e, Dict.get "target" e ) of
    ( Just src, Just dst ) -> Dict.update dst ( appendSource src ) acc
    _ -> acc

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
  div [ style "display" "none" ] []
