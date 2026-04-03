{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.UserDeckView where

import Data.Time (UTCTime, getCurrentTime)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple (Query, Only(..))
import Models.UserDeckView (UserDeckView(..))
import Models.Watermelon (DatabaseTime(..))
import Models.Deck (Deck(..))
import Repo.Utils (notNull, one, orMinTime, removePrefix)
import Repo.Classes
import qualified Repo.UserCardView
import Control.Monad.IO.Class (MonadIO(liftIO))

returnFields :: Query
returnFields = " num_cards_today, new_cards_today, last_study_date, cards_per_day, num_cards_learnt, is_author, user_id, id, name, is_public, description, color, num_cards_total "

markString :: String -> String -> String
markString username id = username ++ id

mark :: UserDeckView -> UserDeckView
mark udv = udv { udvId = markString udv.userId udv.udvId }

unmark :: UserDeckView -> UserDeckView
unmark udv = udv { udvId = removePrefix udv.userId udv.udvId}

unmarkString :: String -> String -> String
unmarkString = removePrefix 

markAll :: [UserDeckView] -> [UserDeckView]
markAll = map mark

unmarkAll :: [UserDeckView] -> [UserDeckView]
unmarkAll = map unmark

updatedSince :: MonadDB m  => String -> Maybe UTCTime -> m [UserDeckView]
updatedSince username since = unmarkAll <$> runQuery query (orMinTime since, orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE last_modified > ? AND created_at < ? AND user_id = ?"

createdSince :: MonadDB m  => String -> Maybe UTCTime -> m [UserDeckView]
createdSince username since = unmarkAll <$> runQuery query (orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE created_at > ? AND user_id = ?"

deletedSince :: MonadDB m => String -> Maybe UTCTime -> m [String]
deletedSince username since = unmarkUsernameAll <$> do
    now <- liftIO getCurrentTime
    runQuery query (orMinTime since, fromMaybe now since, username)
  where
    query :: Query
    query = "SELECT id FROM deleted_udvs WHERE deleted_at > ? AND created_at <= ? AND user_id = ?"
    unmarkUsername = removePrefix username
    unmarkUsernameAll = map (unmarkUsername . fromOnly)

modified :: MonadDB m => String -> UTCTime -> [UserDeckView] -> m Bool
modified username since cards = do
  items :: [UserDeckView] <- runQuery query (since, since, username)
  return (notNull items)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE (created_at > ? OR last_modified > ?) AND user_id = ?"

insertOrUpdate :: MonadDB m => UTCTime -> UserDeckView -> m UserDeckView
insertOrUpdate time unmarked = one =<< runQuery query 
    (deck.udvId, deck.userId 
    , deck.numCardsToday, deck.newCardsToday, deck.lastStudyDate, deck.isAuthor
    , deck.cardsPerDay, deck.numCardsLearnt
    , deck.name, deck.isPublic, deck.description
    , deck.color, deck.numCardsTotal, time, time)
  where
    query :: Query
    query = "INSERT INTO user_deck_views \
    \(id, user_id, num_cards_today, new_cards_today, last_study_date, \
    \is_author, cards_per_day, num_cards_learnt, \
    \ name, is_public, description, color, num_cards_total, \
    \ created_at, last_modified) \
    \VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT (id) \
    \DO UPDATE SET \
    \user_id = EXCLUDED.user_id \
    \, num_cards_today = EXCLUDED.num_cards_today \
    \, new_cards_today = EXCLUDED.new_cards_today \
    \, last_study_date = EXCLUDED.last_study_date \
    \, is_author = EXCLUDED.is_author \
    \, cards_per_day = EXCLUDED.cards_per_day \
    \, num_cards_learnt = EXCLUDED.num_cards_learnt \
    \, name = EXCLUDED.name \
    \, is_public = EXCLUDED.is_public \
    \, description = EXCLUDED.description \
    \, color = COALESCE(EXCLUDED.color, user_deck_views.color) \
    \, num_cards_total = EXCLUDED.num_cards_total \
    \, last_modified = EXCLUDED.last_modified \
    \RETURNING" <> returnFields
    deck = mark unmarked

findCardsOfUDV :: MonadDB m => String -> m [String]
findCardsOfUDV markedId = do
  rows :: [Only String] <- runQuery
          "SELECT id FROM user_card_views WHERE user_deck_id = ?"
          (Only markedId)
  return $ map fromOnly rows

delete :: MonadDB m => String -> UTCTime -> String -> m ()
delete username deletedAt id = do
  createdRows <- runQuery "SELECT created_at FROM user_deck_views WHERE id = ?" (Only markedId)
  case createdRows of
    [] -> pure ()
    [DatabaseTime createdAt] -> do
      execute "DELETE FROM decks WHERE user_deck_id = ?" (Only markedId)
      ucv_ids <- findCardsOfUDV markedId
      mapM_ (Repo.UserCardView.delete username deletedAt . unmarkString username) ucv_ids
      execute "DELETE FROM user_deck_views WHERE id = ?" (Only markedId)
      execute "INSERT INTO deleted_udvs (id, user_id, created_at, deleted_at) VALUES (?, ?, ?, ?)" (markedId, username, createdAt, deletedAt)
      pure ()
    _ -> pure ()
  where
    markedId = markString username id

authored :: UserDeckView -> Bool
authored view = view.isAuthor

userDeckToDeck :: UserDeckView -> Deck
userDeckToDeck unmarked  = Deck 0 name isPublic description color numCardsTotal userId udvId
  where
    (UserDeckView numCardsToday newCardsToday lastStudyDate cardsPerDay numCardsLearnt isAuthor userId udvId name isPublic description color numCardsTotal) = mark unmarked
