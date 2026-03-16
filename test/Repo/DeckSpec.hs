{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.DeckSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Control.Monad (forM_)
import Database.PostgreSQL.Simple (Connection)

import TestHelpers
import Repo.Deck
import Repo.User
import Repo.Classes (execute)
import Models.Deck
import Models.DeckSearch
import Models.User
import Models.Card

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

insertDeckWithCards :: Connection -> String -> String -> String -> Bool -> [String] -> IO Deck
insertDeckWithCards conn username udvId deckName public movesList = do
  _ <- runTestApp conn $ do
    _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
      (udvId, username, deckName, public, length movesList :: Int)
    return ()

  let deck = (mkTestDeck 0 deckName username udvId)
        { Models.Deck.isPublic = public
        , Models.Deck.numCardsTotal = fromIntegral $ length movesList
        }
  inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck)

  _ <- runTestApp conn $ do
    forM_ (zip [1 :: Int ..] movesList) $ \(idx, moves) ->
      execute
        "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (udvId ++ "_card_" ++ show idx, username, udvId, moves, deckName ++ " Card " ++ show idx, "wh" :: String, 0 :: Integer)
    return ()

  return inserted

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
        inserted.color `shouldBe` Nothing
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

    it "preserves an existing color when an old client omits it on update" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "colorauthor" "color@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color) VALUES (?, ?, ?, ?, ?, ?)"
            ("udv_color" :: String, "colorauthor" :: String, "Original" :: String, True, 5 :: Integer, "wh" :: String)
          return ()

        let deck1 = (mkTestDeck 0 "Original Name" "colorauthor" "udv_color") { Models.Deck.color = Just "wh" }
        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck1)

        let deck2 = mkTestDeck 0 "Updated Name" "colorauthor" "udv_color"
        updated <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate deck2)

        inserted.color `shouldBe` Just "wh"
        updated.color `shouldBe` Just "wh"

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

        let query = CardQuery Nothing 10 (Models.Deck.deckId inserted) Nothing
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

        let query = CardQuery Nothing 3 (Models.Deck.deckId inserted) Nothing
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

        let query = CardQuery Nothing 1000 (Models.Deck.deckId inserted) Nothing
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck query

        -- Should not fail, just limit to 100
        _ <- expectRight result
        return ()

  describe "listContinuations" $ do
    it "returns distinct next moves for a single deck prefix" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "contuser" "cont@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        _ <- insertDeckWithCards conn "contuser" "udv_cont" "Continuation Deck" True
          [ "e4 e5 Nf3"
          , "e4 e5 Nc3"
          , "e4 c5"
          , "d4 d5"
          ]

        result <- runTestApp conn $ Repo.Deck.listContinuations "udv_cont" "e4 e5"
        continuations <- expectRight result

        continuations `shouldBe` ["Nc3", "Nf3"]

  describe "searchContinuations" $ do
    it "returns continuations and deck counts for public decks only" $ do
      withCleanDb $ \conn -> do
        let publicUserA = mkTestUser "searchconta" "searchconta@example.com" "password"
        let publicUserB = mkTestUser "searchcontb" "searchcontb@example.com" "password"
        let privateUser = mkTestUser "searchcontc" "searchcontc@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert publicUserA
        _ <- runTestApp conn $ Repo.User.insert publicUserB
        _ <- runTestApp conn $ Repo.User.insert privateUser

        _ <- insertDeckWithCards conn "searchconta" "udv_pub_a" "Sicilian" True
          ["e4 c5", "e4 e5 Nf3", "d4 d5"]
        _ <- insertDeckWithCards conn "searchcontb" "udv_pub_b" "French" True
          ["e4 e6", "e4 e5 Nc3"]
        _ <- insertDeckWithCards conn "searchcontc" "udv_priv" "Caro-Kann" False
          ["e4 c6", "e4 e5 Nf3 Nc6"]

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "e5" 2
          , SearchContinuation "c5" 1
          , SearchContinuation "e6" 1
          ]
        map (.name) (decks response) `shouldBe` ["French", "Sicilian"]
        map (.isPublic) (decks response) `shouldBe` [True, True]

    it "counts exact matches and prefix matches, but only yields continuations after the prefix" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "exactprefix" "exactprefix@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- insertDeckWithCards conn "exactprefix" "udv_exact" "Exact Prefix Deck" True
          [ "e4 e5"
          , "e4 e5 Nf3"
          , "e4 e5 Nc3"
          , "e4 c5"
          ]

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4 e5" Nothing Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "Nc3" 1
          , SearchContinuation "Nf3" 1
          ]
        map (.name) (decks response) `shouldBe` ["Exact Prefix Deck"]

    it "limits only the deck list when limitDecks is set" $ do
      withCleanDb $ \conn -> do
        let userA = mkTestUser "limitconta" "limitconta@example.com" "password"
        let userB = mkTestUser "limitcontb" "limitcontb@example.com" "password"
        let userC = mkTestUser "limitcontc" "limitcontc@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert userA
        _ <- runTestApp conn $ Repo.User.insert userB
        _ <- runTestApp conn $ Repo.User.insert userC

        _ <- insertDeckWithCards conn "limitconta" "udv_limit_a" "Alpha" True
          ["e4 c5", "e4 e5 Nf3", "e4 d5"]
        _ <- insertDeckWithCards conn "limitcontb" "udv_limit_b" "Beta" True
          ["e4 e6", "e4 c6"]
        _ <- insertDeckWithCards conn "limitcontc" "udv_limit_c" "Gamma" True
          ["e4 g6"]

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" (Just 1) Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "c5" 1
          , SearchContinuation "c6" 1
          , SearchContinuation "d5" 1
          , SearchContinuation "e5" 1
          , SearchContinuation "e6" 1
          , SearchContinuation "g6" 1
          ]
        map (.name) (decks response) `shouldBe` ["Alpha"]

    it "limits continuations independently when limitContinuations is set" $ do
      withCleanDb $ \conn -> do
        let userA = mkTestUser "limitmovea" "limitmovea@example.com" "password"
        let userB = mkTestUser "limitmoveb" "limitmoveb@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert userA
        _ <- runTestApp conn $ Repo.User.insert userB

        _ <- insertDeckWithCards conn "limitmovea" "udv_move_a" "Move Alpha" True
          ["e4 e5", "e4 e5 Nf3", "e4 c5"]
        _ <- insertDeckWithCards conn "limitmoveb" "udv_move_b" "Move Beta" True
          ["e4 e5 Nc3", "e4 e6"]

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing (Just 2)
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "e5" 3
          , SearchContinuation "c5" 1
          ]
        map (.name) (decks response) `shouldBe` ["Move Alpha", "Move Beta"]

    it "treats non-positive limits as returning no rows for that section" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "zerolimit" "zerolimit@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- insertDeckWithCards conn "zerolimit" "udv_zero" "Zero Limit Deck" True
          ["e4 e5", "e4 c5"]

        zeroResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" (Just 0) (Just 0)
        zeroResponse <- expectRight zeroResult
        continuations zeroResponse `shouldBe` []
        decks zeroResponse `shouldBe` []

        negativeResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" (Just (-5)) (Just (-5))
        negativeResponse <- expectRight negativeResult
        continuations negativeResponse `shouldBe` []
        decks negativeResponse `shouldBe` []
