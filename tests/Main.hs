{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Aeson                     ( decode )
import qualified Codec.Binary.UTF8.String      as UTF8
import qualified Data.ByteString.Lazy          as BS
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( findByExtension
                                                , goldenVsStringDiff
                                                )
import           System.FilePath                ( takeBaseName
                                                , replaceExtension
                                                )

import           Language.Python.Common         ( prettyText )

import           Language.Python.JSONComposer

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  jsonFiles <- findByExtension [".json"] "./tests"
  return $ testGroup
    "JSONComposer Golden Tests"
    [ goldenVsStringDiff (takeBaseName jsonFile)
                     (\ref new -> ["diff", "-u", ref, new])
                     pyFile
                     (BS.readFile jsonFile >>= pipeline)
    | jsonFile <- jsonFiles
    , let pyFile = replaceExtension jsonFile ".py"
    ]

pipeline :: BS.ByteString -> IO BS.ByteString
pipeline contents =
  let json = decode @JSONPyComposition contents
      py   = fmap prettyText . parse <$> json
  in  pure $ maybe
        (error "JSON decode failed")
        (either (error "JSONComposer parse failed") (BS.pack . UTF8.encode))
        py
