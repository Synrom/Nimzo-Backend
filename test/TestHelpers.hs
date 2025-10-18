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
  _ <- execute_ conn "DELETE FROM decks"
  _ <- execute_ conn "DELETE FROM user_card_views"
  _ <- execute_ conn "DELETE FROM deleted_ucvs"
  _ <- execute_ conn "DELETE FROM deleted_udvs"
  _ <- execute_ conn "DELETE FROM user_deck_views"
  _ <- execute_ conn "DELETE FROM users"
  return ()

-- | Run a test with a clean database
withCleanDb :: (Connection -> IO a) -> IO a
withCleanDb action = withTestDb $ \conn -> do
  cleanTestDb conn
  action conn

-- | Create a test environment
mkTestEnv :: Connection -> IO Env
mkTestEnv conn = do
  jwtKey <- generateKey
  let jwtCfg = defaultJWTSettings jwtKey
  let mailCfg = Google "testuser" "testpass" "Test User" "test@example.com" "http://localhost/verify"
  return $ Env conn jwtCfg mailCfg

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
  , Models.Deck.numCardsTotal = 0
  , Models.Deck.author = author
  , Models.Deck.user_deck_id = userDeckId
  }

-- | Create a test user deck view
mkTestUserDeckView :: String -> String -> String -> UserDeckView
mkTestUserDeckView viewId uid deckName = UserDeckView
  { Models.UserDeckView.numCardsToday = 0
  , Models.UserDeckView.cardsPerDay = 20
  , Models.UserDeckView.numCardsLearnt = 0
  , Models.UserDeckView.isAuthor = True
  , Models.UserDeckView.userId = uid
  , Models.UserDeckView.udvId = viewId
  , Models.UserDeckView.name = deckName
  , Models.UserDeckView.isPublic = True
  , Models.UserDeckView.description = "Test description"
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