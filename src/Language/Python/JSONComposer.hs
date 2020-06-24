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
parse j =
  let g               = toGraph j
      assignmentOrder = topsort g
      mkAssignment ctx = do
        let NodeLabel { identifier, value } = lab' ctx
            inputs                          = lpre' ctx
        (lhs, _) <- parseExpr identifier ""
        (rhs, _) <- parseExpr value ""
        if null inputs
          then return (Assign [lhs] rhs SpanEmpty)
          else do
            exprs <- traverse (parseArgs g . fst) (sortOn snd inputs)
            let args = (`ArgExpr` SpanEmpty) . fst <$> exprs
            return (Assign [lhs] (Call rhs args SpanEmpty) SpanEmpty)
  in  Module <$> traverse (mkAssignment . context g) assignmentOrder
  where parseArgs g = (`parseExpr` "") . identifier . fromJust . lab g

data NodeLabel = NodeLabel
  { identifier :: String
  , value      :: String
  } deriving (Eq, Ord, Show)

toGraph :: JSONPyComposition -> Gr NodeLabel Int
toGraph JSONPyComposition { nodes, edges } =
  let nodeId = (M.!) (reverseMap $ M.fromList $ zip [0 ..] $ M.keys nodes)
      labeledEdges =
          [ (nodeId src, nodeId dst, i)
          | (dst, srcs) <- M.toList edges
          , (i  , src ) <- zip [0 ..] srcs
          ]
      labeledNodes = zip [0 ..] (uncurry NodeLabel <$> M.toList nodes)
  in  insEdges labeledEdges $ insNodes labeledNodes empty
  where reverseMap = M.fromList . fmap swap . M.toList
