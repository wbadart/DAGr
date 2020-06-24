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
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors
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
main = do
  app <- toWaiApp HelloWorld
  run 3000 $ allowCors app

-- ==========

allowCors :: Application -> Application
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = simpleCorsResourcePolicy
  { corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  }
