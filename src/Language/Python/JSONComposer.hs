{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Python.JSONComposer where

import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                )
import qualified Data.Aeson.Types              as JSON
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Language.Python.Common
import           Language.Python.Version3

data JsonPy = JsonPy
  { expr :: String
  , args :: Map String Next
  } deriving (Show, Eq, Generic, FromJSON)

data Next = PyExpr String | Downstream JsonPy
  deriving (Show, Eq, Generic)

instance FromJSON Next where
  parseJSON (JSON.String value) = pure (PyExpr (T.unpack value))
  parseJSON o                   = withObject "Downstream" (\_ -> parseJSON o) o

-- |
parse :: JsonPy -> Either ParseError (Expr SrcSpan)
parse JsonPy { expr, args } = do
  (expr, _) <- parseExpr expr ""
  args      <- traverse (uncurry parseArgs) (M.toList args)
  return (Call (Paren expr SpanEmpty) args SpanEmpty)
 where
  parseArgs param (PyExpr expr) = do
    (expr, _) <- parseExpr expr ""
    return case param of
      "__args"   -> ArgVarArgsPos expr SpanEmpty
      "__kwargs" -> ArgVarArgsKeyword expr SpanEmpty
      _          -> ArgKeyword (Ident param SpanEmpty) expr SpanEmpty
  parseArgs param (Downstream jsonpy) = do
    argExpr <- parse jsonpy
    return (ArgKeyword (Ident param SpanEmpty) argExpr SpanEmpty)
