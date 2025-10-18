{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.UserCardView where

import Data.Time (UTCTime (..), getCurrentTime)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query, Only(..))
import Models.UserCardView (UserCardView(..), NumCorrectTrials (..))
import Models.Watermelon (DatabaseTime(..))
import Repo.Classes
import Repo.Utils (notNull, one, orMinTime, removePrefix)
import Control.Monad.IO.Class (MonadIO(liftIO))

returnFields :: Query
returnFields = " num_correct_trials, next_request, user_id, user_deck_id, id, moves, title, color "

markString :: String -> String -> String
markString username id = username ++ id

mark :: UserCardView -> UserCardView
mark ucv = ucv { 
  ucvId = markString ucv.userId ucv.ucvId,
  userDeckId = markString ucv.userId ucv.userDeckId 
  }

unmark :: UserCardView -> UserCardView
unmark ucv = ucv { 
  ucvId = removePrefix ucv.userId ucv.ucvId,
  userDeckId = removePrefix ucv.userId ucv.userDeckId
  }

markAll :: [UserCardView] -> [UserCardView]
markAll = map mark

unmarkAll :: [UserCardView] -> [UserCardView]
unmarkAll = map unmark

updatedSince :: MonadDB m  => String -> Maybe UTCTime -> m [UserCardView]
updatedSince username since = unmarkAll <$> runQuery query (orMinTime since, orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE last_modified > ? AND created_at < ? AND user_id = ?"

createdSince :: MonadDB m  => String -> Maybe UTCTime -> m [UserCardView]
createdSince username since = unmarkAll <$> runQuery query (orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE created_at > ? AND user_id = ?"

deletedSince :: MonadDB m => String -> Maybe UTCTime -> m [String]
deletedSince username since = unmarkUsernameAll <$> do
    now <- liftIO getCurrentTime
    runQuery query (orMinTime since, fromMaybe now since , username)
  where
    query :: Query
    query = "SELECT id FROM deleted_ucvs WHERE deleted_at > ? AND created_at <= ? AND user_id = ?"
    unmarkUsername = removePrefix username
    unmarkUsernameAll = map (unmarkUsername . fromOnly)

modified :: MonadDB m => String -> UTCTime -> [UserCardView] -> m Bool
modified username since cards = do
  items :: [UserCardView] <- runQuery query (since, since, username)
  return (notNull items)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE (created_at > ? OR last_modified > ?) AND user_id = ?"

find :: MonadDB m => String -> m UserCardView
find id = one =<< runQuery query (Only id)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE id = ?"


insertOrUpdate :: MonadDB m => UTCTime -> UserCardView -> m UserCardView
insertOrUpdate time unmarked = one =<< runQuery query (card.ucvId , card.userDeckId, card.numCorrectTrials, card.nextRequest, card.moves, card.title, card.color, card.userId, time, time)
  where
    query :: Query
    query = "INSERT INTO user_card_views \
    \(id, user_deck_id, \
    \num_correct_trials, next_request, \
    \ moves, title, color, user_id, \
    \ last_modified, created_at) \
    \VALUES (?,?,?,?,?,?,?,?,?,?) ON CONFLICT (id) \
    \DO UPDATE SET \
    \user_deck_id = EXCLUDED.user_deck_id \
    \, user_id = EXCLUDED.user_id \
    \, num_correct_trials = EXCLUDED.num_correct_trials \
    \, moves = EXCLUDED.moves \
    \, title = EXCLUDED.title \
    \, color = EXCLUDED.color \
    \, next_request = EXCLUDED.next_request \
    \, last_modified = EXCLUDED.last_modified \
    \RETURNING" <> returnFields
    card = mark unmarked

isFeasible :: Integer -> Integer -> Integer -> Integer -> Bool
isFeasible now _ _ t | t <= 0 = True
isFeasible now p n _ | n < p  = False
isFeasible now prevRequest nextRequest trials = feasibleNrTrials && feasibleLastTry
  where
    lowerBound 0     = 0
    lowerBound 1     = 1000 * 60 * 15
    lowerBound 2     = 1000 * 60 * 15 + lowerBound 1
    lowerBound 3     = 1000 * 60 * 60 + lowerBound 2
    lowerBound 4     = 1000 * 60 * 60 * 24 + lowerBound 3
    lowerBound n     = 43200000 * n * (n-1) - 426600000
    lastInterval     = (trials-1) * 1000 * 60 * 60 * 24 
    feasibleNrTrials = (nextRequest - prevRequest) >= lowerBound trials
    feasibleLastTry  = (nextRequest - lastInterval) <= now


infeasibleUpdated :: MonadDB m => Integer -> UserCardView -> m Bool
infeasibleUpdated now unmarked = do
  card <- find proposal.ucvId
  let trials = proposal.numCorrectTrials - card.numCorrectTrials
  return $ not $ isFeasible now card.nextRequest proposal.nextRequest trials
  where
    proposal = mark unmarked

infeasibleCreated :: Integer -> Integer -> UserCardView -> Bool
infeasibleCreated now lastPulledAt card = not $ isFeasible now lastPulledAt card.nextRequest card.numCorrectTrials

delete :: MonadDB m => String -> UTCTime -> String -> m ()
delete username deleteAt id = do 
  DatabaseTime createdAt  <- one =<< runQuery "DELETE FROM user_card_views WHERE id = ? RETURNING created_at" (Only $ markString username id)
  execute "INSERT INTO deleted_ucvs (id, user_id, created_at, deleted_at) VALUES (?, ?, ?, ?)" (markString username id, username, createdAt, deleteAt)
  return ()