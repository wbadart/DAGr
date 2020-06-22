module Main where

import qualified Codec.Binary.UTF8.String      as UTF8
import           Data.Aeson                     ( eitherDecode )
import qualified Data.ByteString.Lazy          as BS
import           Language.Python.Common.Pretty  ( prettyText )

import           Language.Python.JSONComposer

main :: IO ()
main =
  BS.interact
    $ BS.pack
    . UTF8.encode
    . either id (either show prettyText . parse)
    . eitherDecode
