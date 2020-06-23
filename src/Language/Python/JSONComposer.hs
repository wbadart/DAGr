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

-- | Try to parse a 'JSONPyComposition' to a Python 3 'Expr'.
parse :: JSONPyComposition -> Either ParseError (Expr SrcSpan)
parse JSONPyComposition { callableExpr, args } = do
  (expr, _)  <- parseExpr callableExpr ""
  parsedArgs <- traverse (uncurry parseArgs) (M.toList args)
  return (Call (Paren expr SpanEmpty) parsedArgs SpanEmpty)
 where
  parseArgs param (Expression argExpr) = do
    (expr, _) <- parseExpr argExpr ""
    return case param of
      "__args"   -> ArgVarArgsPos expr SpanEmpty
      "__kwargs" -> ArgVarArgsKeyword expr SpanEmpty
      _          -> ArgKeyword (Ident param SpanEmpty) expr SpanEmpty
  parseArgs param (Another jsonpy) = do
    argExpr <- parse jsonpy
    return (ArgKeyword (Ident param SpanEmpty) argExpr SpanEmpty)

-- | A 'JSONPyComposition' models a Python 3 call graph, specifying nested
-- callables and their arguments. For example,
--
-- > JSONPyComposition "merge" (M.fromList
-- >   [ ("foo", Another (JSONPyComposition "get_stream0" mempty))
-- >   , ("__args", Expression "[stream1, stream2]")
-- >   ])
--
-- corresponds to the Python function call @merge(*[stream1, stream2],
-- foo=get_stream0())@. See 'args' and 'CompositionArg' for further details.
-- N.B. 'JSONPyComposition' has a 'FromJSON' instance that allows you to
-- express the above with the JSON object:
--
-- > {
-- >   "callableExpr": "merge",
-- >   "args": {
-- >     "foo":    {"callableExpr": "get_stream0", "args": {}},
-- >     "__args": "[stream1, stream2]"
-- >   }
-- > }
data JSONPyComposition = JSONPyComposition
  { callableExpr :: String                     -- ^ An literal Python expression which evaluates to
                                               -- a callable
  , args         :: Map String CompositionArg  -- ^ The keyword arguments to that callable
                                               -- (reserved keys @"__args"@ and @"__kwargs"@
                                               -- specify star expansion)
  } deriving (Show, Eq, Generic, FromJSON)

-- | The values of the 'args' to a 'JSONPyComposition' can either be a 'String'
-- conatining a Python expression or another 'JSONPyComposition' object,
-- representing a nested call.
data CompositionArg
  = Expression String          -- ^ A literal Python expression which will be supplied as the argument
  | Another JSONPyComposition  -- ^ Specifies a nested call
  deriving (Show, Eq, Generic)

instance FromJSON CompositionArg where
  parseJSON (JSON.String value) = pure (Expression (T.unpack value))
  parseJSON v                   = withObject "Another" (\_ -> Another <$> parseJSON v) v
