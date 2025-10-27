{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server
import Network.Wai.Middleware.Cors
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import App.API (api, endpoints)
import App.Env (Env(..))
import App.Server (mkServer)
import App.Options (provideOptions)
import App.Config 

main :: IO ()
main = do
  mailconfig <- loadMailConfig
  dbUrl <- loadDbUrl
  conn <-connectPostgreSQL $ pack dbUrl
  jwtCfg <- loadJWT
  origin <- loadWebOrigin
  let cookie = defaultCookieSettings
      env    = Env { dbConn = conn, jwtSettings = jwtCfg, mailConfig = mailconfig }
      ctx    = jwtCfg :. cookie :. EmptyContext
  withStdoutLogger $ \logger -> do
    let settings = setPort 8080 $ setLogger logger defaultSettings -- TODO: make port configurable
    let policy = simpleCorsResourcePolicy {corsOrigins = Just ([origin], True)}
    putStrLn "Running server on port 8080 ..."
    runSettings settings $ cors (const $ Just policy) $ provideOptions endpoints $ serveWithContext api ctx (mkServer env)