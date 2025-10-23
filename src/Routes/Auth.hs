{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}



module Routes.Auth where

import qualified Data.Text as T
import Servant (throwError, type (:<|>) (..), ServerError(..), err404, err401, type (:>), JSON, ReqBody, JSON, Post, )
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.ByteString.Char8 as C
import Control.Monad.Except
import Data.ByteString.Lazy.UTF8 (toString)
import Servant.Auth.Server 
import Control.Monad.Reader
import Control.Monad
import App.Auth
import Models.User
import Repo.User
import App.AppM
import App.Mail
import Repo.Classes
import Repo.Utils
import App.Error (AppError(..))

type API = 
    "auth" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] NewUser
    :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] NewUser
    :<|> "auth" :> "refresh" :> ReqBody '[JSON] AuthTokenRequest :> Post '[JSON] AuthTokens

type Server = 
  (AuthRequest -> AppM NewUser)
  :<|> (User -> AppM NewUser)
  :<|> (AuthTokenRequest -> AppM AuthTokens)

unauthorized :: AppError
unauthorized = Unauthorized "Username/Email or password is wrong."

invalidToken :: AppError
invalidToken = Unauthorized "Invalid token."

createUser :: User -> AppM NewUser
createUser user = do
  s <- liftIO generateSalt
  let pwdhash = hashWithSalt s user.password
  dbuser <- Repo.User.insert $ user {Models.User.password = pwdhash, salt = s}
  tokens <- createTokens dbuser
  verification_token <- createUserVerification dbuser
  sendVerificationMail user.username user.email verification_token
  return $ newUser tokens dbuser

authCheck :: AuthRequest -> AppM NewUser
authCheck (AuthRequest uname pwd) = do
  user <- Repo.User.find uname >>= orThrow unauthorized
  let ok = hashWithSalt user.salt (T.pack pwd) == user.password
  ensure unauthorized ok
  tokens <- createTokens user
  pure $ newUser tokens user

refreshToken :: AuthTokenRequest -> AppM AuthTokens
refreshToken (AuthTokenRequest reftoken) = do
  jwtCfg <- askJwtCfg
  rt:: RefreshTokenContent <- liftIO (verifyJWT jwtCfg (C.pack reftoken)) >>= orThrow invalidToken
  tokenExpires <- addUTCTime (secondsToNominalDiffTime 1800) <$> liftIO getCurrentTime
  let accessTokenContent = AUser rt.username rt.premium tokenExpires
  accessToken <- liftIO (makeJWT accessTokenContent jwtCfg (Just tokenExpires)) >>= rightOrThrow invalidToken
  return $ AuthTokens (toString accessToken) reftoken tokenExpires

server :: Server
server = authCheck :<|> createUser :<|> refreshToken