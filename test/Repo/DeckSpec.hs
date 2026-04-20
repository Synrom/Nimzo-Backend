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
import qualified Models.DeckDetails
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

    it "returns enriched search metadata fields" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "metaauthor" "metaauthor@example.com" "password"
        let rater = mkTestUser "metarater" "metarater@example.com" "password"
        let importerA = mkTestUser "metaimportera" "metaimportera@example.com" "password"
        let importerB = mkTestUser "metaimporterb" "metaimporterb@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert rater
        _ <- runTestApp conn $ Repo.User.insert importerA
        _ <- runTestApp conn $ Repo.User.insert importerB

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color, is_author) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("udv_meta" :: String, "metaauthor" :: String, "Metadata Deck" :: String, True, 2 :: Integer, "w" :: String, True)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, is_author) VALUES (?, ?, ?, ?, ?, ?)"
            ("metaimporteraudv_meta" :: String, "metaimportera" :: String, "Metadata Deck Copy" :: String, False, 0 :: Integer, False)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, is_author) VALUES (?, ?, ?, ?, ?, ?)"
            ("metaimporterbudv_meta" :: String, "metaimporterb" :: String, "Metadata Deck Copy 2" :: String, False, 0 :: Integer, False)
          return ()

        let sourceDeck = (mkTestDeck 0 "Metadata Deck" "metaauthor" "udv_meta") { Models.Deck.color = Just "w" }
        insertedDeck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate sourceDeck)

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("meta_card_1" :: String, "metaauthor" :: String, "udv_meta" :: String, "e4 c5 Nf3 d6" :: String, "Meta Card 1" :: String, "w" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("meta_card_2" :: String, "metaauthor" :: String, "udv_meta" :: String, "e4 c5 Nc3 e6" :: String, "Meta Card 2" :: String, "w" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO deck_ratings (deck_id, user_id, rating) VALUES (?, ?, ?)"
            (Models.Deck.deckId insertedDeck, "metarater" :: String, 5 :: Int)
          return ()

        result <- runTestApp conn $ Repo.Deck.search (Just "Metadata")
        found <- expectRight result

        length found `shouldBe` 1
        let deck = head found
        deck.name `shouldBe` "Metadata Deck"
        deck.numCardsTotal `shouldBe` 2
        deck.previewMoves `shouldBe` "e4 c5"
        deck.repertoire `shouldBe` "White repertoire"
        deck.rating `shouldBe` Just 5.0
        deck.ratingCount `shouldBe` 1
        deck.downloadCount `shouldBe` 2

  describe "saveRating" $ do
    it "upserts a user's rating and keeps one rating per user/deck" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "ratingauthor" "ratingauthor@example.com" "password"
        let rater = mkTestUser "ratingrater" "ratingrater@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert rater

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color, is_author) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("udv_rating" :: String, "ratingauthor" :: String, "Rating Deck" :: String, True, 0 :: Integer, "b" :: String, True)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Rating Deck" "ratingauthor" "udv_rating"))

        _ <- runTestApp conn $ Repo.Deck.saveRating "ratingrater" (Models.Deck.deckId deck) 4
        _ <- runTestApp conn $ Repo.Deck.saveRating "ratingrater" (Models.Deck.deckId deck) 2

        found <- expectRight =<< runTestApp conn (Repo.Deck.search (Just "Rating Deck"))
        length found `shouldBe` 1
        let ratedDeck = head found
        ratedDeck.rating `shouldBe` Just 2.0
        ratedDeck.ratingCount `shouldBe` 1

    it "rejects ratings outside range 1..5" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "ratingauthor2" "ratingauthor2@example.com" "password"
        let rater = mkTestUser "ratingrater2" "ratingrater2@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert rater

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_rating2" :: String, "ratingauthor2" :: String, "Rating Deck 2" :: String, True, 0 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Rating Deck 2" "ratingauthor2" "udv_rating2"))
        result <- runTestApp conn $ Repo.Deck.saveRating "ratingrater2" (Models.Deck.deckId deck) 6
        result `shouldSatisfy` isLeft'

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

  describe "findWithRating" $ do
    it "returns hasRated=False for anonymous requests" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "detailsuser1" "details1@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_details1" :: String, "detailsuser1" :: String, "Details Deck" :: String, True, 2 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Details Deck" "detailsuser1" "udv_details1"))
        details <- expectRight =<< runTestApp conn (Repo.Deck.findWithRating Nothing (Models.Deck.deckId deck))
        Models.DeckDetails.hasRated details `shouldBe` False

    it "returns hasRated=True when current user rated the deck" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "detailsauthor" "detailsauthor@example.com" "password"
        let rater = mkTestUser "detailsrater" "detailsrater@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert rater

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_details2" :: String, "detailsauthor" :: String, "Details Deck 2" :: String, True, 3 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Details Deck 2" "detailsauthor" "udv_details2"))
        _ <- runTestApp conn $ Repo.Deck.saveRating "detailsrater" (Models.Deck.deckId deck) 5

        details <- expectRight =<< runTestApp conn (Repo.Deck.findWithRating (Just "detailsrater") (Models.Deck.deckId deck))
        Models.DeckDetails.hasRated details `shouldBe` True

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

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing Nothing Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "e5" 2
          , SearchContinuation "c5" 1
          , SearchContinuation "e6" 1
          ]
        map (.deck.name) (decks response) `shouldBe` ["French", "Sicilian"]
        map (.deck_nr_cards) (decks response) `shouldBe` [2, 2]
        map (.deck.isPublic) (decks response) `shouldBe` [True, True]

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

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4 e5" Nothing Nothing Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "Nc3" 1
          , SearchContinuation "Nf3" 1
          ]
        map (.deck.name) (decks response) `shouldBe` ["Exact Prefix Deck"]
        map (.deck_nr_cards) (decks response) `shouldBe` [3]

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

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing (Just 1) Nothing
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "c5" 1
          , SearchContinuation "c6" 1
          , SearchContinuation "d5" 1
          , SearchContinuation "e5" 1
          , SearchContinuation "e6" 1
          , SearchContinuation "g6" 1
          ]
        map (.deck.name) (decks response) `shouldBe` ["Alpha"]
        map (.deck_nr_cards) (decks response) `shouldBe` [3]

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

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing Nothing (Just 2)
        response <- expectRight result

        continuations response `shouldBe`
          [ SearchContinuation "e5" 3
          , SearchContinuation "c5" 1
          ]
        map (.deck.name) (decks response) `shouldBe` ["Move Alpha", "Move Beta"]
        map (.deck_nr_cards) (decks response) `shouldBe` [3, 2]

    it "treats non-positive limits as returning no rows for that section" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "zerolimit" "zerolimit@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- insertDeckWithCards conn "zerolimit" "udv_zero" "Zero Limit Deck" True
          ["e4 e5", "e4 c5"]

        zeroResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing (Just 0) (Just 0)
        zeroResponse <- expectRight zeroResult
        continuations zeroResponse `shouldBe` []
        decks zeroResponse `shouldBe` []

        negativeResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing (Just (-5)) (Just (-5))
        negativeResponse <- expectRight negativeResult
        continuations negativeResponse `shouldBe` []
        decks negativeResponse `shouldBe` []

    it "filters continuation search by deck color" $ do
      withCleanDb $ \conn -> do
        let whiteUser = mkTestUser "colorwhite" "colorwhite@example.com" "password"
        let blackUser = mkTestUser "colorblack" "colorblack@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert whiteUser
        _ <- runTestApp conn $ Repo.User.insert blackUser

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color) VALUES (?, ?, ?, ?, ?, ?)"
            ("udv_white" :: String, "colorwhite" :: String, "White Deck" :: String, True, 2 :: Integer, "w" :: String)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color) VALUES (?, ?, ?, ?, ?, ?)"
            ("udv_black" :: String, "colorblack" :: String, "Black Deck" :: String, True, 2 :: Integer, "b" :: String)
          return ()

        let whiteDeck = (mkTestDeck 0 "White Deck" "colorwhite" "udv_white") { Models.Deck.color = Just "w" }
        let blackDeck = (mkTestDeck 0 "Black Deck" "colorblack" "udv_black") { Models.Deck.color = Just "b" }
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate whiteDeck
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate blackDeck

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("white_1" :: String, "colorwhite" :: String, "udv_white" :: String, "e4 e5" :: String, "White 1" :: String, "w" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("white_2" :: String, "colorwhite" :: String, "udv_white" :: String, "e4 c5" :: String, "White 2" :: String, "w" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("black_1" :: String, "colorblack" :: String, "udv_black" :: String, "e4 e5" :: String, "Black 1" :: String, "b" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("black_2" :: String, "colorblack" :: String, "udv_black" :: String, "e4 e6" :: String, "Black 2" :: String, "b" :: String, 0 :: Integer)
          return ()

        whiteResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" (Just "w") Nothing Nothing
        whiteResponse <- expectRight whiteResult
        continuations whiteResponse `shouldBe`
          [ SearchContinuation "c5" 1
          , SearchContinuation "e5" 1
          ]
        map (.deck.name) (decks whiteResponse) `shouldBe` ["White Deck"]
        map (.deck_nr_cards) (decks whiteResponse) `shouldBe` [2]

        blackResult <- runTestApp conn $ Repo.Deck.searchContinuations "e4" (Just "b") Nothing Nothing
        blackResponse <- expectRight blackResult
        continuations blackResponse `shouldBe`
          [ SearchContinuation "e5" 1
          , SearchContinuation "e6" 1
          ]
        map (.deck.name) (decks blackResponse) `shouldBe` ["Black Deck"]
        map (.deck_nr_cards) (decks blackResponse) `shouldBe` [2]

    it "filters continuation search by color when no prefix is provided" $ do
      withCleanDb $ \conn -> do
        let whiteUser = mkTestUser "noprefixwhite" "noprefixwhite@example.com" "password"
        let blackUser = mkTestUser "noprefixblack" "noprefixblack@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert whiteUser
        _ <- runTestApp conn $ Repo.User.insert blackUser

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color) VALUES (?, ?, ?, ?, ?, ?)"
            ("udv_np_white" :: String, "noprefixwhite" :: String, "No Prefix White" :: String, True, 1 :: Integer, "w" :: String)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total, color) VALUES (?, ?, ?, ?, ?, ?)"
            ("udv_np_black" :: String, "noprefixblack" :: String, "No Prefix Black" :: String, True, 1 :: Integer, "b" :: String)
          return ()

        let whiteDeck = (mkTestDeck 0 "No Prefix White" "noprefixwhite" "udv_np_white") { Models.Deck.color = Just "w" }
        let blackDeck = (mkTestDeck 0 "No Prefix Black" "noprefixblack" "udv_np_black") { Models.Deck.color = Just "b" }
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate whiteDeck
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate blackDeck

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("np_white" :: String, "noprefixwhite" :: String, "udv_np_white" :: String, "e4" :: String, "NP White" :: String, "w" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("np_black" :: String, "noprefixblack" :: String, "udv_np_black" :: String, "d4" :: String, "NP Black" :: String, "b" :: String, 0 :: Integer)
          return ()

        result <- runTestApp conn $ Repo.Deck.searchContinuations "" (Just "w") Nothing Nothing
        response <- expectRight result

        continuations response `shouldBe` [SearchContinuation "e4" 1]
        map (.deck.name) (decks response) `shouldBe` ["No Prefix White"]
        map (.deck_nr_cards) (decks response) `shouldBe` [1]
