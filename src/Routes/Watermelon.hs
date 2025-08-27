{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Routes.Watermelon where

import Data.Proxy
import Data.Time.Clock
import Servant (type (:<|>) (..), ServerError(..), err404, err401, type (:>), ReqBody, JSON, QueryParam, Post, Get, Patch, Capture)
import Servant.Auth.Server (AuthResult(..), throwAll)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import App.AppM
import App.Env
import App.Error
import App.Auth (AuthenticatedUser(..))
import Models.Watermelon (PullParams(..), ChangesResponse(..), TableChanges(..), Changes(..), PushParams(..), Success(..))
import Repo.UserCardView as UserCardView
import Repo.UserDeckView as UserDeckView
import Repo.Deck
import Repo.Utils (ensureM, notNull, flattenChangeset, neitherM)
import Models.UserCardView
import Models.UserDeckView

type API = 
  "changes" :> "pull" :> ReqBody '[JSON] PullParams :> Post '[JSON] ChangesResponse
  :<|> "changes" :> "push" :> ReqBody '[JSON] PushParams :> Post '[JSON] Success

type Server = 
  (PullParams -> AppM ChangesResponse)
  :<|> (PushParams -> AppM Success)

mkChangesFor
  :: Monad m
  => (UTCTime -> m [a])
  -> (UTCTime -> m [a])
  -> UTCTime
  -> m (TableChanges a)
mkChangesFor created updated t =
  TableChanges <$> created t <*> updated t <*> pure []

pullRoute :: PullParams -> AppM ChangesResponse
pullRoute PullParams {lastPulledAt } = do
  now <- liftIO getCurrentTime
  changesUdv <- mkChangesFor UserDeckView.createdSince UserDeckView.updatedSince lastPulledAt
  changesUcv <- mkChangesFor UserCardView.createdSince UserCardView.updatedSince lastPulledAt
  pure $ ChangesResponse 
    { changes   = Changes { usercardview = changesUcv, userdeckview = changesUdv }
    , timestamp = now
    }

mergeError :: AppError
mergeError = MergeConflict "Modified objects after last pull."

unauthorized :: AppError
unauthorized = Unauthorized "You can only modify views of yourself."

validateOwnership :: AuthenticatedUser -> Changes -> AppM ()
validateOwnership user changes =
  if ownsAll (username user) changes then pure ()
  else throwError unauthorized
  where
    allUCVs = flattenChangeset created updated (usercardview changes)
    allUDVs = flattenChangeset created updated (userdeckview changes)
    ownsAll who _ =
         all ((== who) . Models.UserCardView.userId) allUCVs
      && all ((== who) . Models.UserDeckView.userId) allUDVs

pushRoute :: AuthenticatedUser -> PushParams -> AppM Success
pushRoute user PushParams {lastPulledAt, changes} = do
  validateOwnership user changes
  let ucvitems = flattenChangeset created updated (usercardview changes)
      udvitems = flattenChangeset created updated (userdeckview changes)
  ensureM mergeError $ neitherM [
    UserDeckView.modified lastPulledAt udvitems,
    UserCardView.modified lastPulledAt ucvitems ]
  mapM_ UserDeckView.insertOrUpdate udvitems
  mapM_ UserCardView.insertOrUpdate ucvitems
  pure $ Success "Synched successfully."

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = 
  pullRoute 
  :<|> pushRoute user