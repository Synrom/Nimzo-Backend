{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestHelpers where

import Control.Exception (bracket, try, catch, SomeException)
import Control.Monad.Reader
import Control.Monad.Except
import Database.PostgreSQL.Simple
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Environment (lookupEnv)
import Servant.Auth.Server
import Servant (ServerError(..), Handler, runHandler)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import App.Env
import App.AppM
import App.Error
import App.Config
import Models.User (User(..))
import Models.Deck (Deck(..))
import Models.UserDeckView (UserDeckView(..))
import Models.UserCardView (UserCardView(..))
import Models.Watermelon (PullParams (..))

-- | Test database connection string
-- Use a separate test database to avoid conflicts
testDbUrl :: IO String
testDbUrl = do
  maybeUrl <- lookupEnv "TEST_DATABASE_URL"
  case maybeUrl of
    Just url -> return url
    Nothing -> return "postgresql://postgres:postgres@localhost:5432/gambit_test"

-- | Create a test database connection
mkTestConn :: IO Connection
mkTestConn = do
  url <- testDbUrl
  connectPostgreSQL $ pack url

-- | Run an action with a test database connection that is cleaned up afterwards
withTestDb :: (Connection -> IO a) -> IO a
withTestDb = bracket mkTestConn close

-- | Clean all tables in the test database
cleanTestDb :: Connection -> IO ()
cleanTestDb conn = do
  _ <- execute_ conn "DELETE FROM user_onboarding_preferences"
  _ <- execute_ conn "DELETE FROM decks"
  _ <- execute_ conn "DELETE FROM user_card_views"
  _ <- execute_ conn "DELETE FROM deleted_ucvs"
  _ <- execute_ conn "DELETE FROM deleted_udvs"
  _ <- execute_ conn "DELETE FROM user_identities"
  _ <- execute_ conn "DELETE FROM user_deck_views"
  _ <- execute_ conn "DELETE FROM users"
  return ()

ensureTestSchema :: Connection -> IO ()
ensureTestSchema conn = do
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS user_onboarding_preferences (\
    \ user_id VARCHAR(250) PRIMARY KEY REFERENCES users(username) ON DELETE CASCADE,\
    \ chess_level VARCHAR(50) NOT NULL,\
    \ elo VARCHAR(50) NOT NULL,\
    \ organization VARCHAR(50) NOT NULL,\
    \ motivation VARCHAR(250) NOT NULL,\
    \ study_goal VARCHAR(50) NOT NULL,\
    \ last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\
    \ created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\
    \)"
  _ <- execute_ conn "ALTER TABLE user_deck_views ADD COLUMN IF NOT EXISTS color VARCHAR(2)"
  _ <- execute_ conn "ALTER TABLE user_deck_views ADD COLUMN IF NOT EXISTS new_cards_today INTEGER NOT NULL DEFAULT 0"
  _ <- execute_ conn "ALTER TABLE user_deck_views ADD COLUMN IF NOT EXISTS last_study_date VARCHAR(10) NOT NULL DEFAULT ''"
  _ <- execute_ conn "ALTER TABLE decks ADD COLUMN IF NOT EXISTS color VARCHAR(2)"
  _ <- execute_ conn
    "ALTER TABLE decks \
    \ADD COLUMN IF NOT EXISTS search_vector tsvector \
    \GENERATED ALWAYS AS ( \
    \  setweight(to_tsvector('english', coalesce(name, '')), 'A') || \
    \  setweight(to_tsvector('english', coalesce(description, '')), 'B') \
    \) STORED"
  _ <- execute_ conn "CREATE INDEX IF NOT EXISTS decks_search_vector_idx ON decks USING GIN (search_vector)"
  _ <- execute_ conn
    "ALTER TABLE decks \
    \ADD COLUMN IF NOT EXISTS search_vector_name tsvector \
    \GENERATED ALWAYS AS (to_tsvector('english', name)) STORED"
  _ <- execute_ conn "CREATE INDEX IF NOT EXISTS decks_search_vector_name_idx ON decks USING GIN (search_vector_name)"
  _ <- execute_ conn
    "CREATE TABLE IF NOT EXISTS user_identities (\
    \ id SERIAL PRIMARY KEY,\
    \ username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,\
    \ provider VARCHAR(20) NOT NULL,\
    \ provider_subject VARCHAR(255) NOT NULL,\
    \ email VARCHAR(250),\
    \ email_verified BOOLEAN NOT NULL DEFAULT FALSE,\
    \ created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\
    \ last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\
    \ UNIQUE (provider, provider_subject)\
    \)"
  _ <- execute_ conn "CREATE INDEX IF NOT EXISTS user_identities_username_idx ON user_identities(username)"
  return ()

-- | Run a test with a clean database
withCleanDb :: (Connection -> IO a) -> IO a
withCleanDb action = withTestDb $ \conn -> do
  ensureTestSchema conn
  cleanTestDb conn
  action conn

-- | Create a test environment
mkTestEnv :: Connection -> IO Env
mkTestEnv conn = do
  jwtKey <- generateKey
  let jwtCfg = defaultJWTSettings jwtKey
  let mailCfg = Google "testuser" "testpass" "Test User" "test@example.com" "http://localhost/verify" "http://localhost/change" True
  let socialCfg = SocialAuthConfiguration ["test-google-client"] ["test-apple-client"]
  return $ Env conn jwtCfg mailCfg socialCfg

-- | Run an AppM action in a test environment
runTestApp :: Connection -> AppM a -> IO (Either AppError a)
runTestApp conn action = do
  env <- mkTestEnv conn
  -- Handler is IO (Either ServerError a)
  result <- runHandler (nt env action)
  case result of
    Right val -> return $ Right val
    Left (e :: ServerError) -> return $ Left $ Internal $ TL.unpack $ TLE.decodeUtf8 $ errBody e

-- | Helper to extract Right or fail the test
expectRight :: Show e => Either e a -> IO a
expectRight (Right a) = return a
expectRight (Left e) = error $ "Expected Right but got Left: " ++ show e

expectJust :: Maybe e -> IO e
expectJust (Just e) = return e
expectJust Nothing  = error "Expected Just bot got Nothing"

expectLeft :: Either e a -> IO e
expectLeft (Left e) = return e
expectLeft (Right _) = error "Expected Left but got Right"

-- | Create a test user
mkTestUser :: String -> String -> String -> User
mkTestUser uname email pwd = User
  { Models.User.username = uname
  , Models.User.password = T.pack pwd
  , Models.User.salt = "testsalt12345678"
  , Models.User.premium = False
  , Models.User.xp = 0
  , Models.User.streak = 0
  , Models.User.last_activity = read "2025-01-01 00:00:00 UTC"
  , Models.User.rank = 0
  , Models.User.email = email
  , Models.User.verified = False
  }

-- | Create a test deck
mkTestDeck :: Integer -> String -> String -> String -> Deck
mkTestDeck did dname author userDeckId = Deck
  { Models.Deck.deckId = did
  , Models.Deck.name = dname
  , Models.Deck.isPublic = True
  , Models.Deck.description = "Test deck description"
  , Models.Deck.color = Nothing
  , Models.Deck.numCardsTotal = 0
  , Models.Deck.author = author
  , Models.Deck.user_deck_id = userDeckId
  }

-- | Create a test user deck view
mkTestUserDeckView :: String -> String -> String -> UserDeckView
mkTestUserDeckView viewId uid deckName = UserDeckView
  { Models.UserDeckView.numCardsToday = 0
  , Models.UserDeckView.newCardsToday = 0
  , Models.UserDeckView.lastStudyDate = ""
  , Models.UserDeckView.cardsPerDay = 20
  , Models.UserDeckView.numCardsLearnt = 0
  , Models.UserDeckView.isAuthor = True
  , Models.UserDeckView.userId = uid
  , Models.UserDeckView.udvId = viewId
  , Models.UserDeckView.name = deckName
  , Models.UserDeckView.isPublic = True
  , Models.UserDeckView.description = "Test description"
  , Models.UserDeckView.color = Nothing
  , Models.UserDeckView.numCardsTotal = 0
  }

-- | Create a test user card view
mkTestUserCardView :: String -> String -> String -> String -> UserCardView
mkTestUserCardView cardId uid deckId mvs = UserCardView
  { Models.UserCardView.numCorrectTrials = 0
  , Models.UserCardView.nextRequest = 0
  , Models.UserCardView.userId = uid
  , Models.UserCardView.userDeckId = deckId
  , Models.UserCardView.ucvId = cardId
  , Models.UserCardView.moves = mvs
  , Models.UserCardView.title = "Test Card"
  , Models.UserCardView.color = "wh"
  }


mkTestPullParams :: UTCTime -> PullParams
mkTestPullParams time = PullParams (Just $ floor $ utcTimeToPOSIXSeconds time) 1 Nothing
