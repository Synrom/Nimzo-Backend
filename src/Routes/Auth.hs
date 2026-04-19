{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Char (isAlphaNum)
import Data.List (dropWhileEnd)
import Servant (throwError, type (:<|>) (..), type (:>), JSON, ReqBody, Post, Delete, QueryParam)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Except
import Data.ByteString.Lazy.UTF8 (toString)
import Servant.Auth.Server 
import Control.Monad.Reader
import Control.Monad
import Data.Maybe (isJust)
import App.Auth
import App.SocialAuth
import App.Async
import Models.User
import qualified Repo.Onboarding as Repo.Onboarding
import Models.Onboarding (sessionIdMaxLength)
import Models.SocialAuth
import Models.UserIdentity (UserIdentity(..))
import qualified Repo.User as Repo.User
import qualified Repo.UserIdentity as Repo.UserIdentity
import App.AppM
import App.Mail
import Repo.Classes
import Repo.Utils
import App.Error (AppError(..))
import App.Env (socialAuthConfig)
import Models.Watermelon (Success (Success), JsonableMsg (..))

type API = 
    "auth" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] NewUser
    :<|> "auth" :> "google" :> ReqBody '[JSON] SocialAuthRequest :> Post '[JSON] NewUser
    :<|> "auth" :> "apple" :> ReqBody '[JSON] SocialAuthRequest :> Post '[JSON] NewUser
    :<|> "user" :> QueryParam "onboarding_session_id" String :> ReqBody '[JSON] User :> Post '[JSON] NewUser
    :<|> "user" :> ReqBody '[JSON] AuthTokenRequest :> Delete '[JSON] JsonableMsg
    :<|> "user" :> "reqChange" :> ReqBody '[JSON] UserEmail :> Post '[JSON] JsonableMsg
    :<|> "auth" :> "refresh" :> ReqBody '[JSON] AuthTokenRequest :> Post '[JSON] AuthTokens
    :<|> "verify" :> ReqBody '[JSON] Token :> Post '[JSON] JsonableMsg

type Server = 
  (AuthRequest -> AppM NewUser)
  :<|> (SocialAuthRequest -> AppM NewUser)
  :<|> (SocialAuthRequest -> AppM NewUser)
  :<|> (Maybe String -> User -> AppM NewUser)
  :<|> (AuthTokenRequest -> AppM JsonableMsg)
  :<|> (UserEmail -> AppM JsonableMsg)
  :<|> (AuthTokenRequest -> AppM AuthTokens)
  :<|> (Token -> AppM JsonableMsg)

unauthorized :: AppError
unauthorized = Unauthorized "Username/Email or password is wrong."

invalidToken :: AppError
invalidToken = Unauthorized "Invalid token."

missingSocialEmail :: AppError
missingSocialEmail = Unauthorized "Social login did not provide an email for first-time signup."

invalidUsername :: AppError
invalidUsername = Unauthorized "Invalid username."

invalidPassword :: AppError
invalidPassword = Unauthorized "Password cannot be empty."

invalidOnboardingSession :: AppError
invalidOnboardingSession = Unauthorized "Invalid onboarding session id."

createUser :: User -> AppM NewUser
createUser = createUserWithOnboarding Nothing

createUserWithOnboarding :: Maybe String -> User -> AppM NewUser
createUserWithOnboarding maybeOnboardingSessionId user = do
  let trimmedUsername = trim user.username
  ensure invalidUsername (not (null trimmedUsername))
  ensure invalidPassword (not (T.null user.password))
  let normalizedUser :: User
      normalizedUser =
        User
          trimmedUsername
          user.password
          user.salt
          user.premium
          user.xp
          user.streak
          user.last_activity
          user.rank
          user.email
          user.verified
  s <- liftIO generateSalt
  let pwdhash = hashWithSalt s (Models.User.password normalizedUser)
  dbuser <- Repo.User.insert $ normalizedUser {Models.User.password = pwdhash, salt = s}
  case fmap trim maybeOnboardingSessionId of
    Just sessionId -> do
      ensure invalidOnboardingSession (not (null sessionId))
      ensure invalidOnboardingSession (length sessionId <= sessionIdMaxLength)
      Repo.Onboarding.claimAnonymousForUser dbuser.username sessionId
    Nothing -> pure ()
  tokens <- createTokens dbuser
  forkAppM $ do
    verification_token <- createUserVerification dbuser
    sendVerificationMail normalizedUser.username normalizedUser.email verification_token
  return $ newUser tokens dbuser

authCheck :: AuthRequest -> AppM NewUser
authCheck (AuthRequest uname pwd) = do
  user <- Repo.User.find uname >>= orThrow unauthorized
  let ok = hashWithSalt user.salt (T.pack pwd) == user.password
  ensure unauthorized ok
  tokens <- createTokens user
  pure $ newUser tokens user

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

sanitizeRequestedUsername :: String -> String
sanitizeRequestedUsername = take 250 . map sanitizeChar . trim
  where
    sanitizeChar c
      | isAlphaNum c = c
      | c == '_' || c == '-' || c == '.' = c
      | otherwise = '_'

usernameSeedFromEmail :: String -> String
usernameSeedFromEmail email =
  let localPart = takeWhile (/= '@') email
      candidate = map sanitizeChar localPart
   in case filter (/= '_') candidate of
        [] -> "user"
        _ -> take 250 candidate
  where
    sanitizeChar c
      | isAlphaNum c = c
      | otherwise = '_'

resolveUsername :: Maybe String -> String -> AppM String
resolveUsername (Just requested) _ = do
  let candidate = sanitizeRequestedUsername requested
  when (null candidate) (throwError invalidUsername)
  existing <- Repo.User.findUsername candidate
  case existing of
    Nothing -> pure candidate
    Just _ -> throwError $ AlreadyExists "Username already exists."
resolveUsername Nothing email = findAvailableUsername (usernameSeedFromEmail email) 0

findAvailableUsername :: String -> Int -> AppM String
findAvailableUsername base idx = do
  let candidate
        | idx <= 0 = base
        | otherwise = take 250 (base ++ "_" ++ show idx)
  existing <- Repo.User.findUsername candidate
  case existing of
    Nothing -> pure candidate
    Just _ -> findAvailableUsername base (idx + 1)

ensureVerifiedUser :: User -> AppM User
ensureVerifiedUser user
  | user.verified = pure user
  | otherwise = do
      Repo.User.verify user.username
      let verifiedUser :: User
          verifiedUser =
            User
              user.username
              user.password
              user.salt
              user.premium
              user.xp
              user.streak
              user.last_activity
              user.rank
              user.email
              True
      pure verifiedUser

createSocialUser :: String -> String -> Bool -> AppM User
createSocialUser username email verified = do
  saltValue <- liftIO generateSalt
  rawPassword <- liftIO generateSalt
  let passwordHash = hashWithSalt saltValue rawPassword
  Repo.User.insertSocial username passwordHash saltValue email verified

normalizeOptionalEmail :: Maybe String -> Maybe String
normalizeOptionalEmail maybeEmail =
  let cleaned = fmap trim maybeEmail
   in case cleaned of
        Just value | null value -> Nothing
        _ -> cleaned

completeSocialAuth :: String -> SocialProfile -> Maybe String -> Maybe String -> AppM NewUser
completeSocialAuth providerLabel profile requestedUsername requestedEmail = do
  linkedUser <- Repo.UserIdentity.findUser providerLabel profile.providerSubject
  user <- case linkedUser of
    Just existing -> ensureVerifiedUser existing
    Nothing -> do
      let providerEmail = normalizeOptionalEmail profile.email
      let fallbackEmail = normalizeOptionalEmail requestedEmail
      email <- orThrow missingSocialEmail (providerEmail `mplus` fallbackEmail)
      let providerHasVerifiedEmail = isJust providerEmail && profile.emailVerified
      maybeExisting <- if providerHasVerifiedEmail then Repo.User.findEmail email else pure Nothing
      baseUser <- case maybeExisting of
        Just existing -> ensureVerifiedUser existing
        Nothing -> do
          username <- resolveUsername requestedUsername email
          created <- createSocialUser username email providerHasVerifiedEmail
          when (not providerHasVerifiedEmail) $ forkAppM $ do
            verification_token <- createUserVerification created
            sendVerificationMail created.username created.email verification_token
          pure created
      Repo.UserIdentity.insertOrUpdate $
        UserIdentity
          { username = baseUser.username,
            provider = providerLabel,
            providerSubject = profile.providerSubject,
            providerEmail = providerEmail,
            emailVerified = providerHasVerifiedEmail
          }
      pure baseUser
  tokens <- createTokens user
  pure $ newUser tokens user

googleAuth :: SocialAuthRequest -> AppM NewUser
googleAuth request = do
  cfg <- asks socialAuthConfig
  profileResult <- liftIO $ verifyGoogleToken cfg request.idToken
  profile <- either throwError pure profileResult
  completeSocialAuth "google" profile request.username request.requestEmail

appleAuth :: SocialAuthRequest -> AppM NewUser
appleAuth request = do
  cfg <- asks socialAuthConfig
  profileResult <- liftIO $ verifyAppleToken cfg request.idToken
  profile <- either throwError pure profileResult
  completeSocialAuth "apple" profile request.username request.requestEmail

refreshToken :: AuthTokenRequest -> AppM AuthTokens
refreshToken (AuthTokenRequest reftoken) = do
  jwtCfg <- askJwtCfg
  rt:: RefreshTokenContent <- liftIO (verifyJWT jwtCfg (BS8.pack reftoken)) >>= orThrow invalidToken
  tokenExpires <- addUTCTime (secondsToNominalDiffTime 1800) <$> liftIO getCurrentTime
  let accessTokenContent = AUser rt.username rt.premium tokenExpires
  accessToken <- liftIO (makeJWT accessTokenContent jwtCfg (Just tokenExpires)) >>= rightOrThrow invalidToken
  return $ AuthTokens (toString accessToken) reftoken tokenExpires

deleteUser :: AuthTokenRequest -> AppM JsonableMsg
deleteUser (AuthTokenRequest reftoken) = do
  jwtCfg <- askJwtCfg
  rt:: RefreshTokenContent <- liftIO (verifyJWT jwtCfg (BS8.pack reftoken)) >>= orThrow invalidToken
  Repo.User.delete rt.username
  return $ Msg "Successfully deleted user."

verifyUser :: Token -> AppM JsonableMsg
verifyUser token = do 
  user <- orThrow invalidToken =<< decodeVerificationToken token
  Repo.User.verify user.username 
  return $ Msg "Account verified."

requestChangePwd :: UserEmail -> AppM JsonableMsg
requestChangePwd umail = do
  userid <- Repo.User.getUserID umail
  jwtCfg <- askJwtCfg
  forkAppM $ do
    access_token <- createChangePwdToken userid
    sendChangePasswordMail userid.username umail.email access_token
  return $ Msg "Successfully send change password email."

server :: Server
server = 
  authCheck
  :<|> googleAuth
  :<|> appleAuth
  :<|> createUserWithOnboarding
  :<|> deleteUser 
  :<|> requestChangePwd
  :<|> refreshToken 
  :<|> verifyUser
