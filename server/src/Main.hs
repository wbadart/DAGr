{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Aeson                     ( Result(..) )
import           Data.Functor                   ( (<&>) )
import           Language.Python.JSONComposer   ( parse )
import           Language.Python.Common         ( prettyText )
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR POST
|]

instance Yesod HelloWorld

postHomeR :: Handler String
postHomeR = parseCheckJsonBody <&> \case
  Error   msg  -> msg
  Success json -> either show prettyText (parse json)

main :: IO ()
main = warp 3000 HelloWorld
