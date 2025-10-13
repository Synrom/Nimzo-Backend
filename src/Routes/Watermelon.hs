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
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds, getPOSIXTime)
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
import Repo.User as User
import Repo.UserCardView as UserCardView
import Repo.UserDeckView as UserDeckView
import Repo.Utils (ensureM, notNull, flattenChangeset, neitherM, neither, orThrow, arrayLength, getUTCNow, ensure)
import Models.UserCardView
import Models.UserDeckView
import qualified Repo.Deck as Deck
import Models.Deck (Deck)
import Models.User (User(..))
import Repo.Xp (calcXp)

type API = 
  "changes" :> "pull" :> ReqBody '[JSON] PullParams :> Post '[JSON] ChangesResponse
  :<|> "changes" :> "push" :> ReqBody '[JSON] PushParams :> Post '[JSON] Success

type Server = 
  (PullParams -> AppM ChangesResponse)
  :<|> (PushParams -> AppM Success)

intToTime :: Integer -> UTCTime
intToTime = posixSecondsToUTCTime . fromIntegral

timeToInt :: UTCTime -> Integer
timeToInt = floor . utcTimeToPOSIXSeconds

mkChangesFor
  :: Monad m
  => (Maybe UTCTime -> m [a])
  -> (Maybe UTCTime -> m [a])
  -> (Maybe UTCTime -> m [String])
  -> Maybe UTCTime
  -> m (TableChanges a)
mkChangesFor created updated deleted t =
  TableChanges <$> created t <*> updated t <*> deleted t

pullRoute :: String -> PullParams -> AppM ChangesResponse
pullRoute username PullParams {lastPulledAt } = do
  now <- timeToInt <$> liftIO getCurrentTime
  let since = intToTime <$> lastPulledAt
  changesUdv <- mkChangesFor 
    (UserDeckView.createdSince username) 
    (UserDeckView.updatedSince username)
    (UserDeckView.deletedSince username) 
    since
  changesUcv <- mkChangesFor 
    (UserCardView.createdSince username)
    (UserCardView.updatedSince username)
    (UserCardView.deletedSince username)
    since
  pure $ ChangesResponse 
    { changes   = Changes { user_card_views = changesUcv, user_deck_views = changesUdv }
    , timestamp = now
    }

mergeError :: AppError
mergeError = MergeConflict "Modified objects after last pull."

unauthorized :: AppError
unauthorized = Unauthorized "You can only modify views of yourself."

infeasibleError :: AppError
infeasibleError = Unauthorized "Number of trials is infeasible."

validateOwnership :: AuthenticatedUser -> Changes -> AppM ()
validateOwnership user changes =
  if ownsAll user.username changes then pure ()
  else throwError unauthorized
  where
    allUCVs = flattenChangeset created updated (user_card_views changes)
    allUDVs = flattenChangeset created updated (user_deck_views changes)
    ownsAll who _ =
         all ((== who) . Models.UserCardView.userId) allUCVs
      && all ((== who) . Models.UserDeckView.userId) allUDVs

updateUser :: [UserCardView] -> String -> AppM User
updateUser cards username = do
  olduser <- orThrow unauthorized =<< User.findUsername username
  let active = not (null cards)
  if active 
    then do
      User.updateXP (arrayLength cards) olduser
    else do
      pure olduser

pushRoute :: AuthenticatedUser -> PushParams -> AppM Success
pushRoute user PushParams {lastPulledAt, changes} = do
  let since = intToTime lastPulledAt
  validateOwnership user changes
  let ucvitems = flattenChangeset created updated (user_card_views changes)
      udvitems = flattenChangeset created updated (user_deck_views changes)
  now <- liftIO getUTCNow
  ensureM infeasibleError $ neitherM $ map (UserCardView.infeasibleUpdated now) changes.user_card_views.updated
  ensure infeasibleError $ neither $ map (UserCardView.infeasibleCreated now lastPulledAt) changes.user_card_views.created
  ensureM mergeError $ neitherM [
    UserDeckView.modified user.username since udvitems,
    UserCardView.modified user.username since ucvitems ]
  User {xp, streak} <- updateUser changes.user_card_views.updated user.username
  mapM_ (UserDeckView.insertOrUpdate since) udvitems
  mapM_ (UserCardView.insertOrUpdate since) ucvitems
  mapM_ (UserDeckView.delete user.username) changes.user_deck_views.deleted
  mapM_ (UserCardView.delete user.username) changes.user_card_views.deleted
  -- TODO: do these as a background task
  mapM_ (Deck.insertOrUpdate . UserDeckView.userDeckToDeck) $ filter UserDeckView.authored udvitems
  pure $ Success xp streak "Synched successfully."

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = 
  pullRoute user.username
  :<|> pushRoute user
server _ = throwAppError $ Unauthorized "No access."