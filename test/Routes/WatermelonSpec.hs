{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Routes.WatermelonSpec (spec) where

import Test.Hspec
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad (forM_)

import TestHelpers
import Routes.Watermelon
import Routes.Auth
import Repo.User
import Repo.UserDeckView
import App.Error (AppError(..))
import Repo.UserCardView
import Repo.Utils
import Repo.Classes (execute)
import Models.User
import Models.Watermelon
import Models.UserDeckView
import Models.UserCardView
import App.Auth (AuthenticatedUser(..))
import Servant.Auth.Server (AuthResult(..))
import Servant (type (:<|>) (..))
import qualified Models.UserDeckView as Models
import qualified Models.UserCardView as Repo

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = describe "Routes.Watermelon" $ do

  describe "pullRoute" $ do
    it "returns empty changes when no data exists" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pulluser" "pull@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "pulluser" params
        response <- expectRight result

        timestamp response `shouldSatisfy` (> 0)
        created response.changes.user_deck_views `shouldBe` []
        created response.changes.user_card_views `shouldBe` []

    it "returns created items since last pull" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pulluser2" "pull2@example.com" "password"
        now <- getCurrentTime
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert a user deck view directly
        let deck = mkTestUserDeckView "udv_pull1" "pulluser2" "Pull Deck"

        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "pulluser2" params
        response <- expectRight result

        length response.changes.user_deck_views.created `shouldBe` 1
        let deck = head response.changes.user_deck_views.created
        name deck `shouldBe` "Pull Deck"

    it "filters changes by lastPulledAt timestamp" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pulluser3" "pull3@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert old deck
        let olddeck = mkTestUserDeckView "udv_old" "pulluser3" "Old Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate minTime olddeck

        -- Insert new deck
        let newdeck = mkTestUserDeckView "udv_new" "pulluser3" "New Deck"
        now <- getCurrentTime
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now newdeck

        -- Pull with a recent timestamp (should only get new deck)
        now <- getCurrentTime
        let recentPast = floor $ utcTimeToPOSIXSeconds now - 60 -- 60 seconds ago
        let params = PullParams (Just recentPast) 1 Nothing

        result <- runTestApp conn $ Routes.Watermelon.pullRoute "pulluser3" params
        response <- expectRight result

        length response.changes.user_deck_views.created `shouldBe` 1
        let deck = head response.changes.user_deck_views.created
        name deck `shouldBe` "New Deck"

  describe "pushRoute" $ do
    it "successfully pushes new user deck views" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pushuser" "push@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let newDeck = mkTestUserDeckView "push_deck1" "pushuser" "Pushed Deck"
        let tableChanges = TableChanges [newDeck] [] []
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "pushuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        msg success `shouldBe` "Synched successfully."

        -- Verify deck was inserted
        decks <- runTestApp conn $ Repo.UserDeckView.createdSince "pushuser" Nothing
        created <- expectRight decks
        length created `shouldBe` 1

    it "successfully pushes new user card views" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pushuser2" "push2@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert deck first
        let deck = mkTestUserDeckView "deck_for_cards" "pushuser2" "Card Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate minTime deck

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let newCard = mkTestUserCardView "card1" "pushuser2" "deck_for_cards" "e2e4 e7e5"
        let cardChanges = TableChanges [newCard] [] []
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = TableChanges [] [] [] }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "pushuser2" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        msg success `shouldBe` "Synched successfully."

        -- Verify card was inserted
        cards <- runTestApp conn $ Repo.UserCardView.createdSince "pushuser2" Nothing
        created <- expectRight cards
        length created `shouldBe` 1

    it "updates existing items" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "updateuser" "update@example.com" "password"
        now <- getCurrentTime
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert initial deck
        let timeCreation = intToTime $ floor (utcTimeToPOSIXSeconds minTime) + 10
        let deck = mkTestUserDeckView "deck_update" "updateuser" "Original Name"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate timeCreation deck

        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let updatedDeck = (mkTestUserDeckView "deck_update" "updateuser" "Updated Name")
              { numCardsTotal = 10 }
        let tableChanges = TableChanges [] [updatedDeck] []
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "updateuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        msg success `shouldBe` "Synched successfully."

        let between = intToTime $ lastPulled - 1
        let after = intToTime $ lastPulled + 1
        let before = minTime

        -- Verify deck was updated
        -- We only get the update if we pull from before lastPulled
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "updateuser" $ mkTestPullParams between
        response <- expectRight result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 1)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 0)

        -- If we pull from lastPulled, we don't get any updates nor creations
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "updateuser" $ mkTestPullParams after
        response <- expectRight result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 0)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 0)

        -- If we pull from minTime, we don't get any updates, but one creation
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "updateuser" $ mkTestPullParams before
        response <- expectRight result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 0)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 1)

    it "handles deleted items" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "deleteuser" "delete@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert deck to delete
        let deck = mkTestUserDeckView "delete_deck" "deleteuser" "To Delete"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate minTime deck

        -- Insert card to delete
        let card = mkTestUserCardView "card1" "deleteuser" "delete_deck" "e2e4 e7e5"
        _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate minTime card

        now <- getCurrentTime
        let deletedAt = floor $ utcTimeToPOSIXSeconds now
        let betweenCreationAndDeletionTime = floor $ utcTimeToPOSIXSeconds now - 10

        let deckChanges = TableChanges [] [] ["delete_deck"]
        let cardChanges = TableChanges [] [] ["card1"]
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = deckChanges }
        let pushParams = PushParams deletedAt changeSet

        let authUser = AUser "deleteuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        msg success `shouldBe` "Synched successfully."

        let pullParams = mkTestPullParams $ intToTime betweenCreationAndDeletionTime
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "deleteuser" pullParams
        success <- expectRight result

        success.changes.user_deck_views.deleted `shouldContain` ["delete_deck"]
        success.changes.user_card_views.deleted `shouldContain` ["card1"]
    
    it "if something is created and deleted after lastPull it should be ignored" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "deleteuser" "delete@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let createdAt = intToTime $ floor $ utcTimeToPOSIXSeconds now - 20
        let deletedAt = floor $ utcTimeToPOSIXSeconds now - 10
        let beforeCreation = floor $ utcTimeToPOSIXSeconds now - 30

        -- Insert deck to delete
        let deck = mkTestUserDeckView "delete_deck" "deleteuser" "To Delete"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate createdAt deck

        -- Insert card to delete
        let card = mkTestUserCardView "card1" "deleteuser" "delete_deck" "e2e4 e7e5"
        _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate createdAt card

        let deckChanges = TableChanges [] [] ["delete_deck"]
        let cardChanges = TableChanges [] [] ["card1"]
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = deckChanges }
        let pushParams = PushParams deletedAt changeSet
        let authUser = AUser "deleteuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result
        msg success `shouldBe` "Synched successfully."

        let pullParams = mkTestPullParams $ intToTime beforeCreation
        result <- runTestApp conn $ Routes.Watermelon.pullRoute "deleteuser" pullParams
        success <- expectRight result

        length success.changes.user_deck_views.deleted `shouldBe` 0
        length success.changes.user_card_views.deleted `shouldBe` 0

    it "fails when user tries to modify someone else's data" $ do
      withCleanDb $ \conn -> do
        let user1 = mkTestUser "user1" "user1@example.com" "password"
        let user2 = mkTestUser "user2" "user2@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user1
        _ <- runTestApp conn $ Repo.User.insert user2

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        -- Try to push deck for user2 while authenticated as user1
        let someoneDeck = mkTestUserDeckView "deck_other" "user2" "Not My Deck"
        let tableChanges = TableChanges [someoneDeck] [] []
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "user1" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams

        result `shouldSatisfy` isLeft'

    it "detects merge conflicts" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "conflictuser" "conflict@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        let createdAt      = intToTime 1000001
        let beforeCreation = 1000000 :: Integer

        -- Insert deck
        let deck = mkTestUserDeckView "deck_conflict" "conflictuser" "Conflicted"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate createdAt deck

        -- Try to push with old lastPulledAt (before the deck was created/modified)
        let updatedDeck = mkTestUserDeckView "deck_conflict" "conflictuser" "My Update"
        let tableChanges = TableChanges [] [updatedDeck] []
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges }
        let pushParams = PushParams beforeCreation changeSet

        now <- getCurrentTime
        let authUser = AUser "conflictuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams

        -- Should fail due to merge conflict
        err <- expectLeft result
        case err of
          Internal msg -> msg `shouldBe` "{\"msg\":\"Modified objects after last pull.\"}"
          _ -> expectationFailure "Test Env should throw everything as internal"

  describe "server with AuthResult" $ do
    it "allows authenticated users to access pull endpoint" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "authpull" "authpull@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let authUser = AUser "authpull" False now
        let authResult = Authenticated authUser

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ do
          let (pullHandler :<|> _) = Routes.Watermelon.server authResult
          pullHandler params

        response <- expectRight result
        timestamp response `shouldSatisfy` (> 0)

    it "denies unauthenticated users" $ do
      withCleanDb $ \conn -> do
        let authResult = Servant.Auth.Server.Indefinite :: AuthResult AuthenticatedUser

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ do
          let (pullHandler :<|> _) = Routes.Watermelon.server authResult
          pullHandler params

        result `shouldSatisfy` isLeft'
