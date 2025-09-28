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
import App.Auth (AuthenticatedUser(..))
import Models.Rank (RankQuery)
import Models.User (UserXP(..), User(..), PublicUser (PublicUser))
import Repo.Rank (listRank)
import Repo.User (findUsername)
import Repo.Utils (orThrow)

type API = 
  "rank" :> ReqBody '[JSON] RankQuery :> Post '[JSON] [UserXP]
  :<|> "user" :> Get '[JSON] PublicUser

type Server = 
  (RankQuery -> AppM [UserXP])
  :<|> AppM PublicUser

toPublic :: User -> PublicUser
toPublic (User username password salt premium xp rank email verified) =
  PublicUser username premium xp rank email verified

getUserRoute :: String -> AppM User
getUserRoute username = findUsername username >>= orThrow userNotFound 
  where
    userNotFound :: AppError
    userNotFound = NotFound $ "Could not find user " ++ username

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = listRank
  :<|> toPublic <$> getUserRoute user.username
server _ = throwAppError $ Unauthorized "No access."