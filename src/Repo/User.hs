{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.User where

import Database.PostgreSQL.Simple (Only(..), Query)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Time (getCurrentTime)
import Control.Exception (tryJust)
import Data.String (fromString)
import Repo.Classes
import Models.Deck (Deck(..))
import Repo.Utils (one, isNextDay, twoOrMoreDaysPassed)
import App.Error (AppError(..))
import Models.User (User(..), UserXP, UserEmail(..), UserID(..))
import Repo.Xp (calcXp)
import App.AppM (AppM)

returnFields :: Query
returnFields = " username, password, salt, premium, xp, streak, last_activity, rank, email, verified "

insert :: MonadDB m => User -> m User
insert user = one =<< runQuery query (user.username, user.password, user.salt, user.email)
  where
    query :: Query
    query = "INSERT INTO users (username, password, salt, email) VALUES (?,?,?,?) RETURNING " <> returnFields

changePwd :: MonadDB m => String -> T.Text -> T.Text -> m ()
changePwd username salt pwd = void $ execute "UPDATE users SET password = ?, salt = ? WHERE username = ?" (pwd, salt, username)

getUserID :: MonadDB m => UserEmail -> m UserID
getUserID (UEmail email) = one =<< runQuery query (Only email)
  where
    query :: Query
    query = "SELECT email, username, premium FROM users WHERE email = ?"

delete :: MonadDB m => String -> m ()
delete username = do
  -- delete all user cards
  execute "DELETE FROM user_card_views WHERE user_id = ?" (Only username)

  -- delete all decks
  execute "DELETE FROM decks WHERE author = ?" (Only username)

  -- delete all user decks
  execute "DELETE FROM user_deck_views WHERE user_id = ?" (Only username)

  -- delete all deleted_udvs
  execute "DELETE FROM deleted_udvs WHERE user_id = ?" (Only username)

  -- delete all deleted_ucvs
  execute "DELETE FROM deleted_ucvs WHERE user_id = ?" (Only username)

  -- delete user
  execute "DELETE FROM users WHERE username = ?" (Only username)
  return ()


find :: MonadDB m => String -> m (Maybe User)
find name = do
  listToMaybe <$> runQuery query (name, name)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM users WHERE username = ? OR email = ?"

findUsername :: MonadDB m => String -> m (Maybe User)
findUsername name = do
  listToMaybe <$> runQuery query (Only name)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM users WHERE username = ?"

nextStreakStamp :: User -> IO User
nextStreakStamp olduser = do
  now <- getCurrentTime
  pure $
    if isNextDay now olduser.last_activity
      then olduser {streak = olduser.streak + 1, last_activity = now}
    else if twoOrMoreDaysPassed now olduser.last_activity
      then olduser {streak = 0, last_activity = now}
    else olduser

updateXP :: Integer -> User -> AppM User
updateXP nrCards olduser = do
  streakedUser <- liftIO $ nextStreakStamp olduser
  let finalUser = streakedUser { xp = olduser.xp + calcXp nrCards streakedUser.streak}
  one =<< runQuery query (finalUser.streak, finalUser.last_activity, finalUser.xp, olduser.username)
  where
    query :: Query
    query = "UPDATE users SET streak = ?, last_activity = ?, xp = ? WHERE username = ? RETURNING" <> returnFields

verify :: MonadDB m => String -> m ()
verify username = void $ execute "UPDATE users SET verified = TRUE WHERE username = ?" (Only username)