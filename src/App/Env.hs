module App.Env where

import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Servant.Auth.Server (JWTSettings)
import Data.Time (NominalDiffTime)
import App.Config (MailConfiguration, SocialAuthConfiguration)
import Data.ByteString.Char8 (pack)

data Env = Env
  { dbConn      :: Connection
  , jwtSettings :: JWTSettings
  , mailConfig  :: MailConfiguration
  , socialAuthConfig :: SocialAuthConfiguration
  , deckImageDir :: FilePath
  , deckImagePublicBase :: String
  }

withConn :: Env -> (Connection -> IO a) -> IO a
withConn env f = f (dbConn env)

mkPool :: String -> IO Connection
mkPool url = connectPostgreSQL (pack url)
