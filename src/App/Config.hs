module App.Config where

import qualified Data.Text as T
import Configuration.Dotenv (load, parseFile)
import qualified System.Environment as Env
import Data.List (dropWhileEnd)
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
    change_pwd_link :: String,
    test :: Bool
  } deriving (Show)

data SocialAuthConfiguration = SocialAuthConfiguration
  { googleClientIds :: [String]
  , appleClientIds :: [String]
  }
  deriving (Show)

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

splitCsv :: String -> [String]
splitCsv raw = filter (not . null) $ map trim $ go raw
  where
    go [] = [""]
    go (',' : xs) = "" : go xs
    go (x : xs) = case go xs of
      [] -> [[x]]
      (y : ys) -> (x : y) : ys

loadOptionalCsv :: String -> IO [String]
loadOptionalCsv name = do
  values <- parseFile ".env"
  load False values
  maybeValue <- Env.lookupEnv name
  pure $ maybe [] splitCsv maybeValue

loadSocialAuthConfig :: IO SocialAuthConfiguration
loadSocialAuthConfig = do
  googleIds <- loadOptionalCsv "GOOGLE_CLIENT_IDS"
  appleIds <- loadOptionalCsv "APPLE_CLIENT_IDS"
  pure $ SocialAuthConfiguration googleIds appleIds

loadMailConfig :: IO MailConfiguration
loadMailConfig = do
  values <- parseFile ".env"
  load False values
  user_name <- Env.getEnv "USERNAME"
  pwd <- Env.getEnv "PASSWORD"
  name_ <- T.pack <$> Env.getEnv "NAME"
  mail_ <- T.pack <$> Env.getEnv "MAIL"
  verify_link <- Env.getEnv "VERIFICATION_LINK"
  changepwd_link <- Env.getEnv "CHANGE_PWD_LINK"
  return $ Google user_name pwd name_ mail_ verify_link changepwd_link False

loadJWT :: IO JWTSettings
loadJWT = do
  values <- parseFile ".env"
  load False values
  secret <- Env.getEnv "JWT_SECRET"
  pure $ defaultJWTSettings $ fromSecret $ pack secret

loadWebOrigins :: IO [ByteString]
loadWebOrigins = do
  values <- parseFile ".env"
  load False values
  maybeOrigins <- Env.lookupEnv "WEB_ORIGINS"
  case maybeOrigins of
    Just raw ->
      pure $ map (pack . trim) $ splitCsv raw
    Nothing -> do
      origin <- Env.getEnv "WEBORIGIN"
      pure [pack origin]

loadDeckImageDir :: IO FilePath
loadDeckImageDir = do
  values <- parseFile ".env"
  load False values
  maybeDir <- Env.lookupEnv "DECK_IMAGE_DIR"
  pure $ case maybeDir of
    Just path | trim path /= "" -> trim path
    _ -> "/app/public/deck-images"

loadDeckImagePublicBase :: IO String
loadDeckImagePublicBase = do
  values <- parseFile ".env"
  load False values
  maybeBase <- Env.lookupEnv "DECK_IMAGE_PUBLIC_BASE"
  pure $ case maybeBase of
    Just base | trim base /= "" -> trim base
    _ -> "/deck-images"
