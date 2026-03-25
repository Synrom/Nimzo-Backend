{-# LANGUAGE TypeOperators #-}
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
{-# LANGUAGE OverloadedRecordDot #-}

module Routes.User where

import Data.Proxy
import Servant (type (:<|>) (..), ServerError(..), err404, err401, type (:>), ReqBody, JSON, QueryParam, Post, Get, Patch, Capture)
import Servant.Auth.Server (AuthResult(..), throwAll)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import App.AppM
import App.Env
import App.Error (AppError(..), throwAppError)
import App.Auth (AuthenticatedUser(..), generateSalt, hashWithSalt)
import Models.Rank (RankQuery)
import Models.User (UserXP(..), User(..), PublicUser (PublicUser), NewPassword (..))
import Models.Onboarding (OnboardingPreferencesPayload)
import Repo.Rank (listRank)
import Repo.User (findUsername, changePwd)
import qualified Repo.Onboarding as OnboardingRepo
import Repo.Utils (orThrow)
import Models.Watermelon (JsonableMsg(Msg))

type API = 
  "rank" :> ReqBody '[JSON] RankQuery :> Post '[JSON] [UserXP]
  :<|> "user" :> Get '[JSON] PublicUser
  :<|> "user" :> "changepwd" :> ReqBody '[JSON] NewPassword :> Post '[JSON] JsonableMsg
  :<|> "user" :> "onboarding" :> ReqBody '[JSON] OnboardingPreferencesPayload :> Post '[JSON] JsonableMsg

type Server = 
  (RankQuery -> AppM [UserXP])
  :<|> AppM PublicUser
  :<|> (NewPassword -> AppM JsonableMsg)
  :<|> (OnboardingPreferencesPayload -> AppM JsonableMsg)

toPublic :: User -> PublicUser
toPublic (User username password salt premium xp streak last_activity rank email verified) =
  PublicUser username premium xp streak rank email verified

getUserRoute :: String -> AppM User
getUserRoute username = findUsername username >>= orThrow userNotFound 
  where
    userNotFound :: AppError
    userNotFound = NotFound $ "Could not find user " ++ username
  
changeUserPwd :: String -> NewPassword -> AppM JsonableMsg
changeUserPwd username pwd = do
  s <- liftIO generateSalt
  let pwdhash = hashWithSalt s pwd.pwd
  changePwd username s pwdhash
  return $ Msg "Successfully updated password."

saveOnboardingPreferences :: String -> OnboardingPreferencesPayload -> AppM JsonableMsg
saveOnboardingPreferences username payload = do
  OnboardingRepo.upsertForUser username payload
  return $ Msg "Successfully saved onboarding preferences."

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = listRank
  :<|> toPublic <$> getUserRoute user.username
  :<|> changeUserPwd user.username
  :<|> saveOnboardingPreferences user.username
server _ = throwAppError $ Unauthorized "No access."
