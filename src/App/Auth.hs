{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Auth where

import GHC.Generics (Generic)
import Data.Time
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Char8 (pack)
import Control.Monad.Reader
import Control.Monad.Except
import Servant.Auth.Server
import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Password.Argon2
import System.Random (newStdGen, randomRs)
import Data.List as L
import Servant (throwError, err500, ServerError(..))
import Repo.Classes
import Models.User (User(..), UserID(..))
import App.Error
import Repo.Utils


-- Claims carried in tokens
data AuthenticatedUser = AUser
  { username :: String
  , premium  :: Bool
  , expires  :: UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON   AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT    AuthenticatedUser
instance FromJWT  AuthenticatedUser

newtype UserVerification = UserVerification
  { username :: String } deriving (Eq, Show, Generic)
instance ToJSON   UserVerification
instance FromJSON UserVerification
instance ToJWT    UserVerification
instance FromJWT  UserVerification
newtype Token = Token { token :: String } deriving (Eq, Show, Generic)
instance FromJSON  Token

data RefreshTokenContent = RTContent
  { username :: String
  , email    :: String
  , premium  :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON   RefreshTokenContent
instance FromJSON RefreshTokenContent
instance ToJWT    RefreshTokenContent
instance FromJWT  RefreshTokenContent

data AuthRequest = AuthRequest { username :: String, password :: String } deriving (Eq, Show, Generic)
instance FromJSON AuthRequest

newtype AuthTokenRequest = AuthTokenRequest { refresh_token :: String } deriving (Eq, Show, Generic)
instance FromJSON AuthTokenRequest

data AuthTokens = AuthTokens { access_token :: String, refresh_token :: String, expires :: UTCTime } deriving (Eq, Show, Generic)
instance ToJSON AuthTokens
instance FromJSON AuthTokens

data NewUser = NewUser
  { auth     :: AuthTokens
  , username :: String
  , premium  :: Bool
  , xp       :: Integer
  , email    :: String
  , verified :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON NewUser

argon2dParams :: Argon2Params
argon2dParams = defaultParams { argon2OutputLength = 32, argon2Salt = 16 }

hashWithSalt :: T.Text -> T.Text -> T.Text
hashWithSalt salt pwd = unPasswordHash $ hashPasswordWithSalt argon2dParams (Salt $ encodeUtf8 salt) (mkPassword pwd)

generateSalt :: IO T.Text
generateSalt = do
  gen <- newStdGen
  let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
  let randomChars = L.take 16 $ randomRs (0, L.length chars - 1) gen
  return $ T.pack $ L.map (chars !!) randomChars

tokenDuration :: NominalDiffTime -- TODO: make configurable
tokenDuration = secondsToNominalDiffTime 86400

changeEmailDuration :: NominalDiffTime 
changeEmailDuration = secondsToNominalDiffTime 86400

failedCreatingTokenError :: AppError
failedCreatingTokenError  = Internal "Failed to create token."

createTokens :: MonadJWTSettings m => User -> m AuthTokens
createTokens user = do
  jwtCfg <- askJwtCfg
  tokenExpires <- addUTCTime tokenDuration <$> liftIO getCurrentTime
  let accessTokenContent = AUser user.username user.premium tokenExpires
  let refreshTokenContent = RTContent user.username user.email user.premium
  accessToken <- liftIO (makeJWT accessTokenContent jwtCfg (Just tokenExpires)) >>= rightOrThrow failedCreatingTokenError
  refreshToken <- liftIO (makeJWT refreshTokenContent jwtCfg Nothing) >>= rightOrThrow failedCreatingTokenError
  return $ AuthTokens (toString accessToken) (toString refreshToken) tokenExpires

createChangePwdToken :: MonadJWTSettings m => UserID -> m String
createChangePwdToken user = do
  jwtCfg <- askJwtCfg
  tokenExpires <- addUTCTime changeEmailDuration <$> liftIO getCurrentTime
  let accessTokenContent = AUser user.username user.premium tokenExpires
  accessToken <- liftIO (makeJWT accessTokenContent jwtCfg (Just tokenExpires)) >>= rightOrThrow failedCreatingTokenError
  return $ toString accessToken


decodeVerificationToken :: MonadJWTSettings m => Token -> m (Maybe UserVerification)
decodeVerificationToken token = do
  jwtCfg <- askJwtCfg
  liftIO $ verifyJWT jwtCfg (pack token.token)

createUserVerification :: MonadJWTSettings m => User -> m String
createUserVerification user = do
  jwtCfg <- askJwtCfg
  let verification = UserVerification user.username
  token <-  liftIO (makeJWT verification jwtCfg Nothing) >>= rightOrThrow failedCreatingTokenError
  return $ toString token

newUser :: AuthTokens -> User -> NewUser
newUser tokens us = NewUser tokens us.username us.premium us.xp us.email us.verified