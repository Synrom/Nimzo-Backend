{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import App.API (api)
import App.Env (Env(..))
import App.Server (mkServer)
import App.Config 

main :: IO ()
main = do
  mailconfig <- loadMailConfig
  dbUrl <- loadDbUrl
  conn <-connectPostgreSQL $ pack dbUrl
  jwtCfg <- loadJWT
  let cookie = defaultCookieSettings
      env    = Env { dbConn = conn, jwtSettings = jwtCfg, mailConfig = mailconfig }
      ctx    = jwtCfg :. cookie :. EmptyContext
  withStdoutLogger $ \logger -> do
    let settings = setPort 8080 $ setLogger logger defaultSettings -- TODO: make port configurable
    putStrLn "Running server on port 8080 ..."
    runSettings settings $ serveWithContext api ctx (mkServer env)