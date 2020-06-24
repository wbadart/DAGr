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
import           Data.Graph.Inductive           ( LEdge
                                                , Node
                                                , lab
                                                , mkGraph
                                                , pre
                                                , topsort
                                                )
import           Data.Graph.Inductive.PatriciaTree
                                                ( Gr )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )
import           Data.Tuple                     ( swap )
import           GHC.Generics                   ( Generic )

import           Language.Python.Common
import           Language.Python.Version3       ( parseExpr )

data JSONPyComposition = JSONPyComposition
  { nodes :: Map String String
  , edges :: Map String [String]
  } deriving (Show, Generic, FromJSON)

parse :: JSONPyComposition -> Either ParseError (Module SrcSpan)
parse j@(JSONPyComposition { nodes, edges }) =
  let g               = toGraph j
      assignmentOrder = topsort g
      mkAssignment node = do
        let identifier = fromJust (lab g node)
            value      = nodes M.! identifier
            isPrim     = length (pre g node) == 0
        (lhs, _) <- parseExpr identifier ""
        (rhs, _) <- parseExpr value ""
        if isPrim
          then return (Assign [lhs] rhs SpanEmpty)
          else do
            exprs <- traverse (`parseExpr` "") (edges M.! identifier)
            let args = ((`ArgExpr` SpanEmpty) . fst) <$> exprs
            return (Assign [lhs] (Call rhs args SpanEmpty) SpanEmpty)
  in  Module <$> traverse mkAssignment assignmentOrder

toGraph :: JSONPyComposition -> Gr String ()
toGraph JSONPyComposition { nodes, edges } =
  let labeledNodes = zip [0 ..] $ M.keys nodes
      labeledEdges = concatMap
        (uncurry mkEdgeList)
        (M.toList (replaceLabels (reverseMap $ M.fromList labeledNodes) edges))
  in  mkGraph labeledNodes labeledEdges
 where
  mkEdgeList :: Node -> [Node] -> [LEdge ()]
  mkEdgeList dst incoming = [ (src, dst, ()) | src <- incoming ]

  replaceLabels :: (Ord a, Ord b) => Map a b -> Map a [a] -> Map b [b]
  replaceLabels table =
    let find = (table M.!) in M.mapKeys find . fmap (fmap find)

  reverseMap :: (Ord a, Ord b) => Map a b -> Map b a
  reverseMap = M.fromList . fmap swap . M.toList
