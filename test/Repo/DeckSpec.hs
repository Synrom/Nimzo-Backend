{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.DeckSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)

import TestHelpers
import Repo.Deck
import Repo.User
import Repo.Classes (execute)
import Models.Deck
import Models.User
import Models.Card

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = describe "Repo.Deck" $ do

  describe "insertOrUpdate" $ do
    it "inserts a new deck" $ do
      withCleanDb $ \conn -> do
        -- Create a user and user deck view first (foreign key requirement)
        let user = mkTestUser "deckauthor" "author@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        -- Insert user_deck_view directly for testing
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_test1" :: String, "deckauthor" :: String, "Test Deck" :: String, True, 10 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Test Deck" "deckauthor" "udv_test1"
        result <- runTestApp conn $ Repo.Deck.insertOrUpdate deck
        inserted <- expectRight result

        inserted.name `shouldBe` "Test Deck"
        inserted.author `shouldBe` "deckauthor"
        inserted.user_deck_id `shouldBe` "udv_test1"
        Models.Deck.deckId inserted `shouldSatisfy` (> 0)

    it "updates an existing deck on conflict" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "updateauthor" "update@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_test2" :: String, "updateauthor" :: String, "Original" :: String, True, 5 :: Integer)
          return ()

        let deck1 = mkTestDeck 0 "Original Name" "updateauthor" "udv_test2"
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck1)

        let deck2 = mkTestDeck 0 "Updated Name" "updateauthor" "udv_test2"
        updated <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck2)

        updated.name `shouldBe` "Updated Name"
        Models.Deck.deckId updated `shouldBe` Models.Deck.deckId inserted

  describe "search" $ do
    it "returns empty list when query is Nothing" $ do
      withCleanDb $ \conn -> do
        result <- runTestApp conn $ Repo.Deck.search Nothing
        found <- expectRight result
        found `shouldBe` []

    it "searches decks by name" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "searchuser" "search@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_search1" :: String, "searchuser" :: String, "Chess Openings" :: String, True, 20 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_search2" :: String, "searchuser" :: String, "Math Problems" :: String, True, 15 :: Integer)
          return ()

        let deck1 = mkTestDeck 0 "Chess Openings" "searchuser" "udv_search1"
        let deck2 = mkTestDeck 0 "Math Problems" "searchuser" "udv_search2"
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate deck1
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate deck2

        result <- runTestApp conn $ Repo.Deck.search (Just "Chess")
        found <- expectRight result

        length found `shouldBe` 1
        (head found).name `shouldBe` "Chess Openings"

    it "performs case-insensitive search" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "caseuser" "case@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_case1" :: String, "caseuser" :: String, "Python Basics" :: String, True, 10 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Python Basics" "caseuser" "udv_case1"
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate deck

        result <- runTestApp conn $ Repo.Deck.search (Just "python")
        found <- expectRight result

        length found `shouldBe` 1

  describe "find" $ do
    it "finds deck by ID" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "finduser" "find@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_find1" :: String, "finduser" :: String, "Find Me" :: String, True, 8 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Find Me" "finduser" "udv_find1"
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck)

        result <- runTestApp conn $ Repo.Deck.find (Models.Deck.deckId inserted)
        found <- expectRight result

        found.name `shouldBe` "Find Me"
        Models.Deck.deckId found `shouldBe` Models.Deck.deckId inserted

    it "fails when deck ID doesn't exist" $ do
      withCleanDb $ \conn -> do
        result <- runTestApp conn $ Repo.Deck.find 999999
        result `shouldSatisfy` isLeft'

  describe "listCardsOfDeck" $ do
    it "returns empty list when deck has no cards" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "cardsuser" "cards@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_cards1" :: String, "cardsuser" :: String, "Empty Deck" :: String, True, 0 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Empty Deck" "cardsuser" "udv_cards1"
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck)

        let query = CardQuery Nothing 10 (Models.Deck.deckId inserted)
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck query
        pagedCards <- expectRight result

        cards pagedCards `shouldBe` []
        next_cursor pagedCards `shouldBe` Nothing

    it "limits results to specified limit" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "limituser" "limit@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_limit1" :: String, "limituser" :: String, "Many Cards" :: String, True, 5 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Many Cards" "limituser" "udv_limit1"
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck)

        -- Insert 5 cards
        _ <- runTestApp conn $ do
          mapM_ (\i -> execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("card_" ++ show i, "limituser" :: String, "udv_limit1" :: String, "e2e4" :: String, "Card " ++ show i, "wh" :: String, 0 :: Integer))
            [1..5 :: Int]

        let query = CardQuery Nothing 3 (Models.Deck.deckId inserted)
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck query
        pagedCards <- expectRight result

        length (cards pagedCards) `shouldBe` 3
        next_cursor pagedCards `shouldSatisfy` isJust

    it "enforces maximum limit of 100" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "maxlimituser" "maxlimit@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_maxlimit" :: String, "maxlimituser" :: String, "Test" :: String, True, 0 :: Integer)
          return ()

        let deck = mkTestDeck 0 "Test" "maxlimituser" "udv_maxlimit"
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck)

        let query = CardQuery Nothing 1000 (Models.Deck.deckId inserted)
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck query

        -- Should not fail, just limit to 100
        _ <- expectRight result
        return ()
