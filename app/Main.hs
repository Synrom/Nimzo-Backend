{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai (Middleware, rawPathInfo, requestMethod, responseStatus)
import Network.Wai.Internal (Response(..))
import Network.HTTP.Types.Status (statusCode, statusMessage)
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import Servant
import Servant.Auth.Server
import Network.Wai.Middleware.Cors
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import App.API (api)
import App.APNS (newAPNSClient)
import App.Env (Env(..))
import App.Server (mkServer)
import App.Config
import Worker.StreakNotification (runWorker)

responseDetails :: Response -> String
responseDetails (ResponseBuilder _ _ builder) =
  truncateBody $ LBS8.unpack $ BB.toLazyByteString builder
responseDetails (ResponseFile _ _ path _) =
  "response file: " ++ path
responseDetails (ResponseStream _ _ _) =
  "<streaming response body>"
responseDetails (ResponseRaw _ _) =
  "<raw response body>"

truncateBody :: String -> String
truncateBody body
  | null body = "<empty response body>"
  | length body > 500 = take 500 body ++ "..."
  | otherwise = body

logErrorResponses :: Middleware
logErrorResponses app req sendResponse =
  app req $ \res -> do
    let status = responseStatus res
    when (statusCode status >= 400) $
      putStrLn $
        "[HTTP " ++ show (statusCode status) ++ "] "
        ++ BS8.unpack (requestMethod req)
        ++ " "
        ++ BS8.unpack (rawPathInfo req)
        ++ " - "
        ++ BS8.unpack (statusMessage status)
        ++ " | "
        ++ responseDetails res
    sendResponse res

startStreakNotificationWorker :: String -> Maybe APNSConfiguration -> IO ()
startStreakNotificationWorker dbUrl maybeConfig = case maybeConfig of
  Nothing -> putStrLn "APNs configuration missing; streak notification worker disabled."
  Just config -> do
    workerId <- maybe "streak-notification-worker" id <$> lookupEnv "NOTIFICATION_WORKER_ID"
    workerConn <- connectPostgreSQL (pack dbUrl)
    client <- newAPNSClient config
    putStrLn "Streak notification worker started."
    _ <- forkIO $ runWorker workerConn client workerId
    pure ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mailconfig <- loadMailConfig
  dbUrl <- loadDbUrl
  socialConfig <- loadSocialAuthConfig
  uploadDir <- loadDeckImageDir
  uploadPublicBase <- loadDeckImagePublicBase
  promotionModerators <- loadDeckPromotionModerators
  maybeAPNSConfig <- loadAPNSConfiguration
  conn <-connectPostgreSQL $ pack dbUrl
  startStreakNotificationWorker dbUrl maybeAPNSConfig
  jwtCfg <- loadJWT
  origins <- loadWebOrigins
  let cookie = defaultCookieSettings
      env    = Env
        { dbConn = conn
        , jwtSettings = jwtCfg
        , mailConfig = mailconfig
        , socialAuthConfig = socialConfig
        , deckImageDir = uploadDir
        , deckImagePublicBase = uploadPublicBase
        , deckPromotionModerators = promotionModerators
        , apnsConfig = maybeAPNSConfig
        }
      ctx    = jwtCfg :. cookie :. EmptyContext
  withStdoutLogger $ \logger -> do
    let settings = setPort 8080 $ setLogger logger defaultSettings -- TODO: make port configurable
    let policy =
          simpleCorsResourcePolicy
            { corsOrigins = Just (origins, True),
              corsRequestHeaders = ["content-type", "authorization"]
            }
    putStrLn "Running server on port 8080 ..."
    runSettings settings $
      logErrorResponses $
      cors (const $ Just policy) $
      serveWithContext api ctx (mkServer env)
