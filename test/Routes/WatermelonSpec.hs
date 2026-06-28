{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Routes.WatermelonSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.String (fromString)
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
import qualified Repo.UserExplanationView as UserExplanationView
import Repo.Utils
import Repo.Classes (execute)
import Models.User
import Models.Watermelon
import Models.UserDeckView
import Models.UserCardView
import Models.UserExplanationView
import App.Auth (AuthenticatedUser(..))
import Servant.Auth.Server (AuthResult(..))
import Servant (type (:<|>) (..))
import qualified Models.UserDeckView as Models
import qualified Models.UserCardView as Repo

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

emptyExplanationChanges :: TableChanges UserExplanationView
emptyExplanationChanges = TableChanges [] [] []

expectPullResponse :: Either AppError Value -> IO ChangesResponse
expectPullResponse result = do
  value <- expectRight result
  case Aeson.fromJSON value of
    Aeson.Success response -> pure response
    Aeson.Error err -> error $ "Expected valid pull response JSON but got: " ++ err

expectObjectField :: String -> KeyMap.KeyMap Value -> IO Value
expectObjectField key obj =
  expectJust (KeyMap.lookup (fromString key) obj)

expectCreatedUserDeckObject :: Value -> IO (KeyMap.KeyMap Value)
expectCreatedUserDeckObject value = do
  let Object root = value
  changesValue <- expectObjectField "changes" root
  let Object changesObj = changesValue
  decksValue <- expectObjectField "user_deck_views" changesObj
  let Object decksObj = decksValue
  createdValue <- expectObjectField "created" decksObj
  case Aeson.fromJSON createdValue of
    Aeson.Success [Object firstDeck] -> pure firstDeck
    Aeson.Success (_ :: [Value]) -> error "Expected exactly one created deck entry"
    Aeson.Error err -> error $ "Expected created deck array but got: " ++ err

expectCreatedUserCardObject :: Value -> IO (KeyMap.KeyMap Value)
expectCreatedUserCardObject value = do
  let Object root = value
  changesValue <- expectObjectField "changes" root
  let Object changesObj = changesValue
  cardsValue <- expectObjectField "user_card_views" changesObj
  let Object cardsObj = cardsValue
  createdValue <- expectObjectField "created" cardsObj
  case Aeson.fromJSON createdValue of
    Aeson.Success [Object firstCard] -> pure firstCard
    Aeson.Success (_ :: [Value]) -> error "Expected exactly one created card entry"
    Aeson.Error err -> error $ "Expected created card array but got: " ++ err

spec :: Spec
spec = describe "Routes.Watermelon" $ do

  describe "pullRoute" $ do
    it "returns empty changes when no data exists" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pulluser" "pull@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "pulluser" params
        response <- expectPullResponse result

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
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "pulluser2" params
        response <- expectPullResponse result

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

        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "pulluser3" params
        response <- expectPullResponse result

        length response.changes.user_deck_views.created `shouldBe` 1
        let deck = head response.changes.user_deck_views.created
        name deck `shouldBe` "New Deck"

    it "omits color from pulled deck entries for schema version 1" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pulllegacy" "pulllegacy@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = (mkTestUserDeckView "udv_legacy" "pulllegacy" "Legacy Deck")
              { Models.color = Just "w" }
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck

        value <- expectRight =<< runTestApp conn (Routes.Watermelon.pullRouteVersioned "pulllegacy" (PullParams Nothing 1 Nothing))
        firstDeck <- expectCreatedUserDeckObject value
        KeyMap.lookup "color" firstDeck `shouldBe` Nothing

    it "includes color in pulled deck entries for schema version 2" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pullmodern" "pullmodern@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = (mkTestUserDeckView "udv_modern" "pullmodern" "Modern Deck")
              { Models.color = Just "w" }
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck

        value <- expectRight =<< runTestApp conn (Routes.Watermelon.pullRouteVersioned "pullmodern" (PullParams Nothing 2 Nothing))
        firstDeck <- expectCreatedUserDeckObject value
        KeyMap.lookup "color" firstDeck `shouldSatisfy` (/= Nothing)
        KeyMap.lookup "new_cards_today" firstDeck `shouldBe` Nothing
        KeyMap.lookup "last_study_date" firstDeck `shouldBe` Nothing

    it "includes new_cards_today and last_study_date in pulled deck entries for schema version 3" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pullv3" "pullv3@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = (mkTestUserDeckView "udv_v3" "pullv3" "V3 Deck")
              { Models.color = Just "b"
              , Models.newCardsToday = 7
              , Models.lastStudyDate = "2026-04-03"
              }
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck

        value <- expectRight =<< runTestApp conn (Routes.Watermelon.pullRouteVersioned "pullv3" (PullParams Nothing 3 Nothing))
        firstDeck <- expectCreatedUserDeckObject value
        KeyMap.lookup "color" firstDeck `shouldSatisfy` (/= Nothing)
        KeyMap.lookup "new_cards_today" firstDeck `shouldSatisfy` (/= Nothing)
        KeyMap.lookup "last_study_date" firstDeck `shouldSatisfy` (/= Nothing)

    it "omits card fen and explanation changes for schema version 3" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pullv3explain" "pullv3explain@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = mkTestUserDeckView "deck_v3_explain" "pullv3explain" "V3 Explain Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck
        let card = (mkTestUserCardView "card_v3_fen" "pullv3explain" "deck_v3_explain" "e2e4")
              { Repo.fen = Just "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1" }
        _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate now card
        let explanation = mkTestUserExplanationView "explanation_v3" "pullv3explain" "deck_v3_explain"
        _ <- runTestApp conn $ UserExplanationView.insertOrUpdate now explanation

        value <- expectRight =<< runTestApp conn (Routes.Watermelon.pullRouteVersioned "pullv3explain" (PullParams Nothing 3 Nothing))
        firstCard <- expectCreatedUserCardObject value
        KeyMap.lookup "fen" firstCard `shouldBe` Nothing
        let Object root = value
        changesValue <- expectObjectField "changes" root
        let Object changesObj = changesValue
        KeyMap.lookup "user_deck_views" changesObj `shouldSatisfy` (/= Nothing)
        KeyMap.lookup "user_card_views" changesObj `shouldSatisfy` (/= Nothing)
        KeyMap.lookup "user_explanation_views" changesObj `shouldBe` Nothing

    it "includes card fen but omits explanation changes for schema version 4" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pullv4fen" "pullv4fen@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = mkTestUserDeckView "deck_v4_fen" "pullv4fen" "V4 Fen Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck
        let fenValue = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
            card = (mkTestUserCardView "card_v4_fen" "pullv4fen" "deck_v4_fen" "e2e4")
              { Repo.fen = Just fenValue }
        _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate now card
        let explanation = mkTestUserExplanationView "explanation_v4_hidden" "pullv4fen" "deck_v4_fen"
        _ <- runTestApp conn $ UserExplanationView.insertOrUpdate now explanation

        value <- expectRight =<< runTestApp conn (Routes.Watermelon.pullRouteVersioned "pullv4fen" (PullParams Nothing 4 Nothing))
        firstCard <- expectCreatedUserCardObject value
        KeyMap.lookup "fen" firstCard `shouldBe` Just (String (fromString fenValue))
        let Object root = value
        changesValue <- expectObjectField "changes" root
        let Object changesObj = changesValue
        KeyMap.lookup "user_explanation_views" changesObj `shouldBe` Nothing

    it "includes explanation changes and card fen for schema version 5" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pullv5explain" "pullv5explain@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        now <- getCurrentTime

        let deck = mkTestUserDeckView "deck_v5_explain" "pullv5explain" "V5 Explain Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate now deck
        let fenValue = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
            card = (mkTestUserCardView "card_v5_fen" "pullv5explain" "deck_v5_explain" "e2e4")
              { Repo.fen = Just fenValue }
        _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate now card
        let UserExplanationView userDeckId userId explanationViewId fen move _ _ =
              mkTestUserExplanationView "explanation_v5" "pullv5explain" "deck_v5_explain"
            explanation = UserExplanationView userDeckId userId explanationViewId fen move "A v5 explanation" "{\"quality\":\"good\"}"
        _ <- runTestApp conn $ UserExplanationView.insertOrUpdate now explanation

        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "pullv5explain" (PullParams Nothing 5 Nothing)
        response <- expectPullResponse result

        length response.changes.user_card_views.created `shouldBe` 1
        (head response.changes.user_card_views.created).fen `shouldBe` Just fenValue
        length response.changes.user_explanation_views.created `shouldBe` 1
        let pulledExplanation = head response.changes.user_explanation_views.created
        pulledExplanation.explanationViewId `shouldBe` "explanation_v5"
        pulledExplanation.userDeckId `shouldBe` "deck_v5_explain"
        pulledExplanation.text `shouldBe` "A v5 explanation"
        pulledExplanation.visualizers `shouldBe` "{\"quality\":\"good\"}"

  describe "pushRoute" $ do
    it "successfully pushes new user deck views" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pushuser" "push@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let newDeck = mkTestUserDeckView "push_deck1" "pushuser" "Pushed Deck"
        let tableChanges = TableChanges [newDeck] [] []
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "pushuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        success.msg `shouldBe` "Synched successfully."

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
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = TableChanges [] [] [], user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "pushuser2" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        success.msg `shouldBe` "Synched successfully."

        -- Verify card was inserted
        cards <- runTestApp conn $ Repo.UserCardView.createdSince "pushuser2" Nothing
        created <- expectRight cards
        length created `shouldBe` 1

    it "rejects user card fen longer than 92 characters" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pushfenlong" "pushfenlong@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        let deck = mkTestUserDeckView "deck_for_long_fen" "pushfenlong" "Long Fen Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate minTime deck

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10
            longFen = replicate 93 'x'
            newCard = (mkTestUserCardView "card_long_fen" "pushfenlong" "deck_for_long_fen" "e2e4")
              { Repo.fen = Just longFen }
            cardChanges = TableChanges [newCard] [] []
            changeSet = Changes { user_card_views = cardChanges, user_deck_views = TableChanges [] [] [], user_explanation_views = emptyExplanationChanges }
            pushParams = PushParams lastPulled changeSet
            authUser = AUser "pushfenlong" False now

        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        err <- expectLeft result
        case err of
          Internal msg -> msg `shouldBe` "{\"msg\":\"Invalid card fen.\"}"
          _ -> expectationFailure "Test Env should throw everything as internal"

    it "successfully pushes new user explanation views" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "pushexplain" "pushexplain@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        let deck = mkTestUserDeckView "deck_for_explanations" "pushexplain" "Explanation Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate minTime deck

        now <- getCurrentTime
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let UserExplanationView userDeckId userId explanationViewId fen move _ _ =
              mkTestUserExplanationView "explanation1" "pushexplain" "deck_for_explanations"
            explanation = UserExplanationView userDeckId userId explanationViewId fen move "Explain the move" "{\"futurePlans\":[\"e4\"]}"
        let explanationChanges = TableChanges [explanation] [] []
        let changeSet = Changes
              { user_card_views = TableChanges [] [] []
              , user_deck_views = TableChanges [] [] []
              , user_explanation_views = explanationChanges
              }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "pushexplain" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        success.msg `shouldBe` "Synched successfully."

        explanations <- runTestApp conn $ UserExplanationView.createdSince "pushexplain" Nothing
        created <- expectRight explanations
        length created `shouldBe` 1
        (head created).explanationViewId `shouldBe` "explanation1"

    it "decodes older push payloads without card fen or explanation changes" $ do
      let payload = "{\"lastPulledAt\":1,\"changes\":{\"user_card_views\":{\"created\":[{\"num_correct_trials\":0,\"next_request_at\":0,\"user_id\":\"legacyuser\",\"user_deck_id\":\"legacydeck\",\"id\":\"legacycard\",\"moves\":\"e2e4\",\"title\":\"Legacy Card\",\"color\":\"wh\"}],\"updated\":[],\"deleted\":[]},\"user_deck_views\":{\"created\":[],\"updated\":[],\"deleted\":[]}}}"
      case Aeson.eitherDecode payload of
        Right (PushParams _ changes) -> do
          changes.user_explanation_views `shouldBe` emptyExplanationChanges
          length changes.user_card_views.created `shouldBe` 1
          (head changes.user_card_views.created).fen `shouldBe` Nothing
        Left err -> expectationFailure err

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
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams lastPulled changeSet

        let authUser = AUser "updateuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        success.msg `shouldBe` "Synched successfully."

        let between = intToTime $ lastPulled - 1
        let after = intToTime $ lastPulled + 1
        let before = minTime

        -- Verify deck was updated
        -- We only get the update if we pull from before lastPulled
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "updateuser" $ mkTestPullParams between
        response <- expectPullResponse result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 1)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 0)

        -- If we pull from lastPulled, we don't get any updates nor creations
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "updateuser" $ mkTestPullParams after
        response <- expectPullResponse result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 0)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 0)

        -- If we pull from minTime, we don't get any updates, but one creation
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "updateuser" $ mkTestPullParams before
        response <- expectPullResponse result
        length response.changes.user_deck_views.updated `shouldSatisfy` (== 0)
        length response.changes.user_deck_views.created `shouldSatisfy` (== 1)

    it "preserves deck color when an older client updates without the new field" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "compatuser" "compat@example.com" "password"
        now <- getCurrentTime
        _ <- runTestApp conn $ Repo.User.insert user
        let originalCreatedAt = intToTime $ floor (utcTimeToPOSIXSeconds now) - 20
        let lastPulled = floor $ utcTimeToPOSIXSeconds now - 10

        let originalDeck = (mkTestUserDeckView "deck_compat" "compatuser" "Original Name")
              { Models.color = Just "wh" }
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate originalCreatedAt originalDeck

        let updatedDeck = (mkTestUserDeckView "deck_compat" "compatuser" "Updated Name")
              { Models.numCardsTotal = 10
              , Models.color = Nothing
              }
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = TableChanges [] [updatedDeck] [], user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams lastPulled changeSet
        let authUser = AUser "compatuser" False now

        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result
        success.msg `shouldBe` "Synched successfully."

        let afterOriginalCreation = intToTime $ floor (utcTimeToPOSIXSeconds originalCreatedAt) + 1
        pulled <- runTestApp conn $ Repo.UserDeckView.updatedSince "compatuser" (Just afterOriginalCreation)
        [storedDeck] <- expectRight pulled
        storedDeck.color `shouldBe` Just "wh"

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
        let cardChanges = TableChanges [] [] []
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = deckChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams deletedAt changeSet

        let authUser = AUser "deleteuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result

        success.msg `shouldBe` "Synched successfully."

        let pullParams = mkTestPullParams $ intToTime betweenCreationAndDeletionTime
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "deleteuser" pullParams
        success <- expectPullResponse result

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
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = deckChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams deletedAt changeSet
        let authUser = AUser "deleteuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result
        success.msg `shouldBe` "Synched successfully."

        let pullParams = mkTestPullParams $ intToTime beforeCreation
        result <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "deleteuser" pullParams
        success <- expectPullResponse result

        length success.changes.user_deck_views.deleted `shouldBe` 0
        length success.changes.user_card_views.deleted `shouldBe` 0

    it "rejects recreating a deleted deck id and pull schemaVersion 3 only returns deleted entry" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "recreatedeck" "recreatedeck@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let createdAt = intToTime $ floor (utcTimeToPOSIXSeconds now) - 40
        let deletedAt = floor (utcTimeToPOSIXSeconds now) - 20
        let betweenCreationAndDeletion = floor (utcTimeToPOSIXSeconds now) - 30

        let originalDeck = mkTestUserDeckView "same_deck_id" "recreatedeck" "Original Deck"
        _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate createdAt originalDeck

        let deleteChanges = Changes
              { user_card_views = TableChanges [] [] []
              , user_deck_views = TableChanges [] [] ["same_deck_id"]
              , user_explanation_views = emptyExplanationChanges
              }
        let authUser = AUser "recreatedeck" False now
        deleteResult <- runTestApp conn $ Routes.Watermelon.pushRoute authUser (PushParams deletedAt deleteChanges)
        _ <- expectRight deleteResult

        let recreatedDeck = mkTestUserDeckView "same_deck_id" "recreatedeck" "Recreated Deck"
        let recreateChanges = Changes
              { user_card_views = TableChanges [] [] []
              , user_deck_views = TableChanges [recreatedDeck] [] []
              , user_explanation_views = emptyExplanationChanges
              }
        recreateResult <- runTestApp conn $ Routes.Watermelon.pushRoute authUser (PushParams deletedAt recreateChanges)
        recreateErr <- expectLeft recreateResult
        case recreateErr of
          Internal msg -> msg `shouldBe` "{\"msg\":\"Cannot recreate a deleted deck id.\"}"
          _ -> expectationFailure "Test Env should throw everything as internal"

        let pullParams = PullParams (Just betweenCreationAndDeletion) 3 Nothing
        pullResult <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "recreatedeck" pullParams
        pullResponse <- expectPullResponse pullResult

        pullResponse.changes.user_deck_views.created `shouldBe` []
        pullResponse.changes.user_deck_views.deleted `shouldContain` ["same_deck_id"]

    it "ignores delete ids that do not exist in the backend" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "missingdelete" "missingdelete@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        now <- getCurrentTime
        let deletedAt = floor $ utcTimeToPOSIXSeconds now

        let deckChanges = TableChanges [] [] ["missing_deck"]
        let cardChanges = TableChanges [] [] ["missing_card"]
        let changeSet = Changes { user_card_views = cardChanges, user_deck_views = deckChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams deletedAt changeSet
        let authUser = AUser "missingdelete" False now

        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams
        success <- expectRight result
        success.msg `shouldBe` "Synched successfully."

        pullResult <- runTestApp conn $ Routes.Watermelon.pullRouteVersioned "missingdelete" (PullParams (Just deletedAt) 2 Nothing)
        pullResponse <- expectPullResponse pullResult

        pullResponse.changes.user_deck_views.deleted `shouldBe` []
        pullResponse.changes.user_card_views.deleted `shouldBe` []

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
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges, user_explanation_views = emptyExplanationChanges }
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
        let changeSet = Changes { user_card_views = TableChanges [] [] [], user_deck_views = tableChanges, user_explanation_views = emptyExplanationChanges }
        let pushParams = PushParams beforeCreation changeSet

        now <- getCurrentTime
        let authUser = AUser "conflictuser" False now
        result <- runTestApp conn $ Routes.Watermelon.pushRoute authUser pushParams

        -- Should fail due to merge conflict
        err <- expectLeft result
        case err of
          Internal msg -> msg `shouldBe` "{\"msg\":\"Modified objects after last pull.\"}"
          _ -> expectationFailure "Test Env should throw everything as internal"


  it "correct feasibility checks" $ do
    withCleanDb $ \conn -> do
      let user = mkTestUser "user" "conflict@example.com" "password"
      _ <- runTestApp conn $ Repo.User.insert user

      let deckCreatedAt   = 1760884622
      let cardCreatedAt   = 1760884630
      let lastPulledAt    = 1760884651
      let now             = 1760884651761
      let origNextRequest = 1760884630525
      let nextRequest     = 1760885551609

      -- create Deck
      let deck = mkTestUserDeckView "deck" "user" "Deck"
      _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate (intToTime deckCreatedAt) deck

      -- add Card and updateDeck
      let card = (mkTestUserCardView "cardId" "user" "deck" "e4 e5") {nextRequest = origNextRequest}
      _ <- runTestApp conn $ Repo.UserCardView.insertOrUpdate (intToTime cardCreatedAt) card
      let updateDeck = deck {numCardsTotal = 1}
      _ <- runTestApp conn $ Repo.UserDeckView.insertOrUpdate (intToTime cardCreatedAt) updateDeck

      -- do push to check card
      let updatedCard = card {numCorrectTrials = 1, nextRequest = nextRequest}
      response <- runTestApp conn $ Repo.UserCardView.infeasibleUpdated now updatedCard
      result <- expectRight response
      result `shouldBe` False

      return ()

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

        response <- expectPullResponse result
        timestamp response `shouldSatisfy` (> 0)

    it "denies unauthenticated users" $ do
      withCleanDb $ \conn -> do
        let authResult = Servant.Auth.Server.Indefinite :: AuthResult AuthenticatedUser

        let params = PullParams Nothing 1 Nothing
        result <- runTestApp conn $ do
          let (pullHandler :<|> _) = Routes.Watermelon.server authResult
          pullHandler params

        result `shouldSatisfy` isLeft'
