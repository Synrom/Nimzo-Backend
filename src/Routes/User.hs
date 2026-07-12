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
import Control.Monad.IO.Class (liftIO)
import Data.List (dropWhileEnd)
import Servant (type (:<|>) (..), type (:>), ReqBody, JSON, Post, Get)
import Servant.Auth.Server (AuthResult(..))
import App.AppM
import App.Error (AppError(..), throwAppError)
import App.Auth (AuthenticatedUser(..), generateSalt, hashWithSalt)
import Models.Feedback (FeedbackPayload(..), feedbackTextMaxLength)
import Models.Rank (RankQuery)
import Models.User (UserXP(..), User(..), PublicUser (PublicUser), NewPassword (..))
import Repo.Rank (listRank)
import Repo.User (findUsername, changePwd)
import Repo.Utils (ensure, orThrow)
import Models.Watermelon (JsonableMsg(Msg))
import qualified Repo.Feedback as FeedbackRepo

type API = 
  "rank" :> ReqBody '[JSON] RankQuery :> Post '[JSON] [UserXP]
  :<|> "user" :> Get '[JSON] PublicUser
  :<|> "user" :> "changepwd" :> ReqBody '[JSON] NewPassword :> Post '[JSON] JsonableMsg
  :<|> "feedback" :> ReqBody '[JSON] FeedbackPayload :> Post '[JSON] JsonableMsg

type Server = 
  (RankQuery -> AppM [UserXP])
  :<|> AppM PublicUser
  :<|> (NewPassword -> AppM JsonableMsg)
  :<|> (FeedbackPayload -> AppM JsonableMsg)

toPublic :: User -> PublicUser
toPublic (User username password salt premium elo xp streak last_activity rank email verified) =
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

invalidFeedback :: AppError
invalidFeedback = Unauthorized "Invalid feedback."

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

saveFeedback :: String -> FeedbackPayload -> AppM JsonableMsg
saveFeedback username payload = do
  let normalizedText = trim payload.text
  ensure invalidFeedback (not (null normalizedText))
  ensure invalidFeedback (length normalizedText <= feedbackTextMaxLength)
  ensure invalidFeedback (payload.stars >= 1 && payload.stars <= 5)
  _ <- FeedbackRepo.insert username (FeedbackPayload normalizedText payload.stars)
  return $ Msg "Successfully saved feedback."

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = listRank
  :<|> toPublic <$> getUserRoute user.username
  :<|> changeUserPwd user.username
  :<|> saveFeedback user.username
server _ = throwAppError $ Unauthorized "No access."
