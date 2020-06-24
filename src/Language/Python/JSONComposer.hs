{-|
Module      : Language.Python.JSONComposer
Description : Parse JSONComposer JSON object to Python expressions
Copyright   : (c) Will Badart, 2020
License     : GPL-3
Maintainer  : will@willbadart.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Python.JSONComposer where

import           Data.Aeson                     ( FromJSON(..) )
import           Data.Graph.Inductive    hiding ( nodes
                                                , edges
                                                )
import           Data.List                      ( sortOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )
import           Data.Tuple                     ( swap )
import           GHC.Generics                   ( Generic )

import           Language.Python.Common  hiding ( empty )
import           Language.Python.Version3       ( parseExpr )

data JSONPyComposition = JSONPyComposition
  { nodes :: Map String String
  , edges :: Map String [String]
  } deriving (Show, Generic, FromJSON)

parse :: JSONPyComposition -> Either ParseError (Module SrcSpan)
parse = parseGraph . toGraph

parseGraph :: Gr (String, String) Int -> Either ParseError (Module SrcSpan)
parseGraph g =
  let assignmentOrder = topsort g
  in  Module <$> traverse (mkAssignment . context g) assignmentOrder
 where
  mkAssignment ctx = do
    let (identifier, value) = lab' ctx
        inputs              = lpre' ctx
    (lhs, _) <- parseExpr identifier ""
    (rhs, _) <- parseExpr value ""
    if null inputs
      then return (Assign [lhs] rhs SpanEmpty)
      else do
        exprs <- traverse (parseArgs . fst) (sortOn snd inputs)
        let args = (`ArgExpr` SpanEmpty) . fst <$> exprs
        return (Assign [lhs] (Call rhs args SpanEmpty) SpanEmpty)
  parseArgs = (`parseExpr` "") . fst . fromJust . lab g

toGraph :: JSONPyComposition -> Gr (String, String) Int
toGraph JSONPyComposition { nodes, edges } =
  let nodeId = (M.!) (reverseMap $ M.fromList $ zip [0 ..] $ M.keys nodes)
      labeledEdges =
          [ (nodeId src, nodeId dst, i)
          | (dst, srcs) <- M.toList edges
          , (i  , src ) <- zip [0 ..] srcs
          ]
      labeledNodes = zip [0 ..] (M.toList nodes)
  in  insEdges labeledEdges $ insNodes labeledNodes empty
  where reverseMap = M.fromList . fmap swap . M.toList
