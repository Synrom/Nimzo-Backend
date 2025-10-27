module App.Config where

import qualified Data.Text as T
import Configuration.Dotenv (load, parseFile)
import qualified System.Environment as Env
import Servant.Auth.Server (fromSecret, JWTSettings, defaultJWTSettings)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

import Database.PostgreSQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword, connectPort,
        connectUser
      ),
    defaultConnectInfo
  )

loadDbUrl :: IO String
loadDbUrl = do
  values <- parseFile ".env"
  load False values
  Env.getEnv "DB_URL"

-- TODO: one single config record

data MailConfiguration = Google
  { username :: String,
    password :: String,
    name :: T.Text,
    mail :: T.Text,
    verification_link :: String,
    test :: Bool
  } deriving (Show)

loadMailConfig :: IO MailConfiguration
loadMailConfig = do
  values <- parseFile ".env"
  load False values
  user_name <- Env.getEnv "USERNAME"
  pwd <- Env.getEnv "PASSWORD"
  name_ <- T.pack <$> Env.getEnv "NAME"
  mail_ <- T.pack <$> Env.getEnv "MAIL"
  link <- Env.getEnv "VERIFICATION_LINK"
  return $ Google user_name pwd name_ mail_ link False

loadJWT :: IO JWTSettings
loadJWT = do
  values <- parseFile ".env"
  load False values
  secret <- Env.getEnv "JWT_SECRET"
  pure $ defaultJWTSettings $ fromSecret $ pack secret

loadWebOrigin :: IO ByteString
loadWebOrigin = do
  values <- parseFile ".env"
  load False values
  pack <$> Env.getEnv "WEBORIGIN"
