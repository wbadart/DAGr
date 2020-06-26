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
                         (pipeline <$> BS.readFile jsonFile)
    | jsonFile <- jsonFiles
    , let pyFile = replaceExtension jsonFile ".py"
    ]

pipeline :: BS.ByteString -> BS.ByteString
pipeline contents =
  let json = decode contents
      py   = fmap prettyText . parseProgram <$> json
  in  maybe (error "JSON decode failed")
            (either (error "JSONComposer parse failed") toBS)
            py
  where toBS = BS.pack . UTF8.encode . unlines . lines
