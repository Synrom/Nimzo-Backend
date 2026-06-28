{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.DeckSpec (spec) where

import Test.Hspec
import Data.List (sort)
import Data.Maybe (isJust)
import Control.Monad (forM_)
import Database.PostgreSQL.Simple (Connection, Only(..), fromOnly, query)

import TestHelpers
import Repo.Deck
import Repo.User
import Repo.Classes (execute)
import Models.Deck
import Models.DeckImage (DeckImageUploadRequest(..))
import Models.DeckPromotion (DeckPromotionRequest(..), DeckPromotionResponse(..))
import qualified Models.DeckImage as DeckImageModel
import qualified Models.DeckDetails
import Models.DeckSearch
import Models.User
import Models.Card
import qualified Models.UserExplanationView as Explanation

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

    it "returns matches for short full-search queries via name fallback" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "shortsearchuser" "shortsearch@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_short1" :: String, "shortsearchuser" :: String, "Sicilian Defense" :: String, True, 10 :: Integer)
          return ()

        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate (mkTestDeck 0 "Sicilian Defense" "shortsearchuser" "udv_short1")

        result <- runTestApp conn $ Repo.Deck.search (Just "Si")
        found <- expectRight result

        length found `shouldBe` 1
        (head found).name `shouldBe` "Sicilian Defense"

    it "matches word-prefix results and ignores inner-word substring-only matches in full search" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "prefixrankuser" "prefixrank@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_rank_sic" :: String, "prefixrankuser" :: String, "Sicilian Defense" :: String, True, 10 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_rank_benoni" :: String, "prefixrankuser" :: String, "Benoni Defense: Classical Variation (white)" :: String, True, 10 :: Integer)
          return ()

        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate (mkTestDeck 0 "Sicilian Defense" "prefixrankuser" "udv_rank_sic")
        _ <- runTestApp conn $ Repo.Deck.insertOrUpdate (mkTestDeck 0 "Benoni Defense: Classical Variation (white)" "prefixrankuser" "udv_rank_benoni")

        result <- runTestApp conn $ Repo.Deck.search (Just "sic")
        found <- expectRight result

        length found `shouldBe` 1
        (head found).name `shouldBe` "Sicilian Defense"

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
          _ <- execute "UPDATE decks SET featured_source = ?, featured_card_id = ? WHERE id = ?"
            (Just ("tiktok" :: String), Just ("meta_card_2" :: String), Models.Deck.deckId insertedDeck)
          return ()

        result <- runTestApp conn $ Repo.Deck.search (Just "Metadata")
        found <- expectRight result

        length found `shouldBe` 1
        let deck = head found
        deck.name `shouldBe` "Metadata Deck"
        deck.numCardsTotal `shouldBe` 2
        deck.previewMoves `shouldBe` "e4 c5"
        deck.repertoire `shouldBe` "White repertoire"
        deck.featuredCardMoves `shouldBe` Just "e4 c5 Nc3 e6"
        deck.rating `shouldBe` Just 5.0
        deck.ratingCount `shouldBe` 1
        deck.downloadCount `shouldBe` 2

  describe "searchInstant" $ do
    it "handles punctuation-heavy queries without SQL tsquery syntax errors" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "instantuser" "instant@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_instant_1" :: String, "instantuser" :: String, "Center Game: Hall Variation (white)" :: String, True, 10 :: Integer)
          return ()

        _ <- runTestApp conn $
          Repo.Deck.insertOrUpdate (mkTestDeck 0 "Center Game: Hall Variation (white)" "instantuser" "udv_instant_1")

        result <- runTestApp conn $
          Repo.Deck.searchInstant (Just "Center Game: Hall Variation (white)")
        found <- expectRight result

        length found `shouldBe` 1
        (head found).name `shouldBe` "Center Game: Hall Variation (white)"

    it "returns all matching H-variations for both full and instant search" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "instantfulluser" "instantfull@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_if_1" :: String, "instantfulluser" :: String, "Center Game: Hall Variation (white)" :: String, True, 10 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_if_2" :: String, "instantfulluser" :: String, "Center Game: Halasz-McDonnell Gambit (white)" :: String, True, 10 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_if_3" :: String, "instantfulluser" :: String, "Center Game: l'Hermet Variation (black)" :: String, True, 10 :: Integer)
          return ()

        _ <- runTestApp conn $
          Repo.Deck.insertOrUpdate (mkTestDeck 0 "Center Game: Hall Variation (white)" "instantfulluser" "udv_if_1")
        _ <- runTestApp conn $
          Repo.Deck.insertOrUpdate (mkTestDeck 0 "Center Game: Halasz-McDonnell Gambit (white)" "instantfulluser" "udv_if_2")
        _ <- runTestApp conn $
          Repo.Deck.insertOrUpdate (mkTestDeck 0 "Center Game: l'Hermet Variation (black)" "instantfulluser" "udv_if_3")

        fullResult <- runTestApp conn $ Repo.Deck.search (Just "Center Game: H")
        instantResult <- runTestApp conn $ Repo.Deck.searchInstant (Just "Center Game: H")
        fullFound <- expectRight fullResult
        instantFound <- expectRight instantResult

        let expectedNames =
              sort
                [ "Center Game: Hall Variation (white)",
                  "Center Game: Halasz-McDonnell Gambit (white)",
                  "Center Game: l'Hermet Variation (black)"
                ]
        sort (map (.name) fullFound) `shouldBe` expectedNames
        sort (map (.name) instantFound) `shouldBe` expectedNames

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

  describe "saveDeckImage" $ do
    it "allows the author to upload a deck image and stores imageUrl" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "imgauthor" "imgauthor@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_img1" :: String, "imgauthor" :: String, "Image Deck" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Image Deck" "imgauthor" "udv_img1"))

        response <- expectRight =<< runTestApp conn (Repo.Deck.saveDeckImage "imgauthor" (Models.Deck.deckId deck) (DeckImageUploadRequest "image/png" "aGVsbG8="))
        DeckImageModel.imageUrl response `shouldSatisfy` (not . null)

    it "rejects deck image upload by non-author" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "imgauthor2" "imgauthor2@example.com" "password"
        let other = mkTestUser "imgother" "imgother@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert other
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_img2" :: String, "imgauthor2" :: String, "Image Deck 2" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Image Deck 2" "imgauthor2" "udv_img2"))

        result <- runTestApp conn $ Repo.Deck.saveDeckImage "imgother" (Models.Deck.deckId deck) (DeckImageUploadRequest "image/png" "aGVsbG8=")
        result `shouldSatisfy` isLeft'

    it "rejects unsupported image mime type" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "imgauthor3" "imgauthor3@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_img3" :: String, "imgauthor3" :: String, "Image Deck 3" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Image Deck 3" "imgauthor3" "udv_img3"))

        result <- runTestApp conn $ Repo.Deck.saveDeckImage "imgauthor3" (Models.Deck.deckId deck) (DeckImageUploadRequest "image/gif" "R0lGODlhAQABAIAAAAUEBA==")
        result `shouldSatisfy` isLeft'

  describe "promotion metadata" $ do
    it "saves and returns valid promotion metadata for the author" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor" "promoauthor@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo1" :: String, "promoauthor" :: String, "Promo Deck" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck" "promoauthor" "udv_promo1"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("promo_card_1" :: String, "promoauthor" :: String, "udv_promo1" :: String, "e4 e5" :: String, "Promo Card 1" :: String, "wh" :: String, 0 :: Integer)
          return ()

        response <- expectRight =<< runTestApp conn
          (Repo.Deck.savePromotion "promoauthor" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") (Just "promo_card_1") Nothing (Just 3) (Just "https://example.com/promo.mp4")))
        let DeckPromotionResponse _ source featuredCardId rank video = response
        source `shouldBe` Just "tiktok"
        featuredCardId `shouldBe` Just "promo_card_1"
        rank `shouldBe` Just 3
        video `shouldBe` Just "https://example.com/promo.mp4"

    it "rejects invalid featured source" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor2" "promoauthor2@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo2" :: String, "promoauthor2" :: String, "Promo Deck 2" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck 2" "promoauthor2" "udv_promo2"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("promo2_card_1" :: String, "promoauthor2" :: String, "udv_promo2" :: String, "d4 d5" :: String, "Promo2 Card 1" :: String, "wh" :: String, 0 :: Integer)
          return ()
        result <- runTestApp conn
          (Repo.Deck.savePromotion "promoauthor2" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "youtube") (Just "promo2_card_1") Nothing (Just 1) Nothing))
        saved <- expectRight result
        let DeckPromotionResponse _ source2 _ _ _ = saved
        source2 `shouldBe` Just "youtube"

    it "rejects invalid video url" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor3" "promoauthor3@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo3" :: String, "promoauthor3" :: String, "Promo Deck 3" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck 3" "promoauthor3" "udv_promo3"))

        result <- runTestApp conn
          (Repo.Deck.savePromotion "promoauthor3" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") (Just "missing_card") Nothing (Just 1) (Just "ftp://example.com/video.mp4")))
        result `shouldSatisfy` isLeft'

    it "allows setting featured source without featuredCardId" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor4" "promoauthor4@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo4" :: String, "promoauthor4" :: String, "Promo Deck 4" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck 4" "promoauthor4" "udv_promo4"))

        response <- expectRight =<< runTestApp conn
          (Repo.Deck.savePromotion "promoauthor4" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") Nothing Nothing (Just 5) Nothing))
        let DeckPromotionResponse _ source featuredCardId rank _ = response
        source `shouldBe` Just "tiktok"
        featuredCardId `shouldBe` Nothing
        rank `shouldBe` Just 5

    it "infers featuredCardId from featuredMoves when provided" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor5" "promoauthor5@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo5" :: String, "promoauthor5" :: String, "Promo Deck 5" :: String, True, 1 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("promo5_card_1" :: String, "promoauthor5" :: String, "udv_promo5" :: String, "e4 c5 Nf3 d6" :: String, "Promo5 Card 1" :: String, "wh" :: String, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck 5" "promoauthor5" "udv_promo5"))

        response <- expectRight =<< runTestApp conn
          (Repo.Deck.savePromotion "promoauthor5" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") Nothing (Just "e4 c5 Nf3 d6") (Just 7) Nothing))
        let DeckPromotionResponse _ _ featuredCardId rank _ = response
        featuredCardId `shouldBe` Just "promo5_card_1"
        rank `shouldBe` Just 7

    it "fails when featuredMoves does not match a card in deck" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promoauthor6" "promoauthor6@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promo6" :: String, "promoauthor6" :: String, "Promo Deck 6" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Deck 6" "promoauthor6" "udv_promo6"))

        result <- runTestApp conn
          (Repo.Deck.savePromotion "promoauthor6" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") Nothing (Just "c4 e5 Nc3") (Just 1) Nothing))
        result `shouldSatisfy` isLeft'

    it "rejects promotion moderation for non-moderators" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "promomodauthor" "promomodauthor@example.com" "password"
        let moderator = mkTestUser "mod1" "mod1@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert moderator
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_promomod" :: String, "promomodauthor" :: String, "Promo Mod Deck" :: String, True, 0 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Promo Mod Deck" "promomodauthor" "udv_promomod"))
        _ <- runTestApp conn $ execute "UPDATE decks SET featured_source = NULL, featured_rank = NULL, video_url = NULL WHERE id = ?" (Only $ Models.Deck.deckId deck)

        result <- runTestApp conn
          (Repo.Deck.savePromotionByModerator "mod1" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") (Just "missing_card") Nothing (Just 2) Nothing))
        result `shouldSatisfy` isLeft'

  describe "listFeatured" $ do
    it "returns only featured decks ordered by newest created_at first and applies limit" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "featureduser" "featureduser@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_featured_a" :: String, "featureduser" :: String, "Featured A" :: String, True, 0 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_featured_b" :: String, "featureduser" :: String, "Featured B" :: String, True, 0 :: Integer)
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_featured_c" :: String, "featureduser" :: String, "Featured C" :: String, True, 0 :: Integer)
          return ()
        deckA <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Featured A" "featureduser" "udv_featured_a"))
        deckB <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Featured B" "featureduser" "udv_featured_b"))
        deckC <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Featured C" "featureduser" "udv_featured_c"))

        _ <- runTestApp conn $ execute "UPDATE decks SET featured_source = 'tiktok', featured_rank = 2 WHERE id = ?" (Only $ Models.Deck.deckId deckA)
        _ <- runTestApp conn $ execute "UPDATE decks SET featured_source = 'tiktok', featured_rank = 1 WHERE id = ?" (Only $ Models.Deck.deckId deckB)
        _ <- runTestApp conn $ execute "UPDATE decks SET featured_source = 'tiktok', featured_rank = 3 WHERE id = ?" (Only $ Models.Deck.deckId deckC)
        _ <- runTestApp conn $ execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
          ("featured_card_b" :: String, "featureduser" :: String, "udv_featured_b" :: String, "e4 e5" :: String, "Featured Card B" :: String, "wh" :: String, 0 :: Integer)
        _ <- runTestApp conn $ execute "UPDATE decks SET featured_card_id = ? WHERE id = ?"
          ("featured_card_b" :: String, Models.Deck.deckId deckB)
        _ <- runTestApp conn $ execute "UPDATE decks SET created_at = ? WHERE id = ?" ("2026-01-01 00:00:00+00" :: String, Models.Deck.deckId deckA)
        _ <- runTestApp conn $ execute "UPDATE decks SET created_at = ? WHERE id = ?" ("2026-02-01 00:00:00+00" :: String, Models.Deck.deckId deckB)
        _ <- runTestApp conn $ execute "UPDATE decks SET created_at = ? WHERE id = ?" ("2026-03-01 00:00:00+00" :: String, Models.Deck.deckId deckC)

        featured <- expectRight =<< runTestApp conn (Repo.Deck.listFeatured (Just "tiktok") (Just 2))
        map (.name) featured `shouldBe` ["Featured C", "Featured B"]
        map (.featuredCardMoves) featured `shouldBe` [Nothing, Just "e4 e5"]

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
        Models.DeckDetails.userRating details `shouldBe` Nothing

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
        Models.DeckDetails.userRating details `shouldBe` Just 5

  describe "listCardsOfDeck" $ do
    it "keeps featured deck response aligned with saved featured card id" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "featuredconsistency" "featuredconsistency@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_featured_consistency" :: String, "featuredconsistency" :: String, "Featured Consistency Deck" :: String, True, 2 :: Integer)
          return ()
        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Featured Consistency Deck" "featuredconsistency" "udv_featured_consistency"))
        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("fcons_card_1" :: String, "featuredconsistency" :: String, "udv_featured_consistency" :: String, "e4 e5" :: String, "FCONS Card 1" :: String, "wh" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("fcons_card_2" :: String, "featuredconsistency" :: String, "udv_featured_consistency" :: String, "d4 d5" :: String, "FCONS Card 2" :: String, "wh" :: String, 0 :: Integer)
          return ()
        _ <- expectRight =<< runTestApp conn
          (Repo.Deck.savePromotion "featuredconsistency" (Models.Deck.deckId deck) (DeckPromotionRequest (Just "tiktok") (Just "fcons_card_2") Nothing (Just 1) Nothing))

        featuredDecks <- expectRight =<< runTestApp conn (Repo.Deck.listFeatured (Just "tiktok") (Just 10))
        let selected = filter (\d -> d.deckId == Models.Deck.deckId deck) featuredDecks
        length selected `shouldBe` 1
        let featuredCardMoves = (head selected).featuredCardMoves
        featuredCardMoves `shouldBe` Just "d4 d5"

        _ <- expectRight =<< runTestApp conn (Repo.Deck.listCardsOfDeck True (DeckContentQuery Nothing 10 (Models.Deck.deckId deck) Nothing Nothing))
        pure ()

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

        let query = DeckContentQuery Nothing 10 (Models.Deck.deckId inserted) Nothing Nothing
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck True query
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

        let query = DeckContentQuery Nothing 3 (Models.Deck.deckId inserted) Nothing Nothing
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck True query
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

        let query = DeckContentQuery Nothing 1000 (Models.Deck.deckId inserted) Nothing Nothing
        result <- runTestApp conn $ Repo.Deck.listCardsOfDeck True query

        -- Should not fail, just limit to 100
        _ <- expectRight result
        return ()

    it "increments deck download_count when cursor is not set" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "downloaduser1" "download1@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_dl_1" :: String, "downloaduser1" :: String, "Download Deck 1" :: String, True, 2 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Download Deck 1" "downloaduser1" "udv_dl_1"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("dl_card_1" :: String, "downloaduser1" :: String, "udv_dl_1" :: String, "e4 e5" :: String, "DL Card 1" :: String, "wh" :: String, 0 :: Integer)
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("dl_card_2" :: String, "downloaduser1" :: String, "udv_dl_1" :: String, "d4 d5" :: String, "DL Card 2" :: String, "wh" :: String, 0 :: Integer)
          return ()

        beforeRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        let cardQuery = DeckContentQuery Nothing 10 (Models.Deck.deckId deck) Nothing Nothing
        _ <- expectRight =<< runTestApp conn (Repo.Deck.listCardsOfDeck True cardQuery)

        afterRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        let beforeCount = fromOnly (head beforeRows)
        let afterCount = fromOnly (head afterRows)
        afterCount `shouldBe` (beforeCount + 1)

    it "does not increment deck download_count when cursor is set" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "downloaduser2" "download2@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_dl_2" :: String, "downloaduser2" :: String, "Download Deck 2" :: String, True, 2 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Download Deck 2" "downloaduser2" "udv_dl_2"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("dl2_card_1" :: String, "downloaduser2" :: String, "udv_dl_2" :: String, "e4 e5" :: String, "DL2 Card 1" :: String, "wh" :: String, 0 :: Integer)
          return ()

        beforeRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        let cardQuery = DeckContentQuery (Just "dl2_card_1") 10 (Models.Deck.deckId deck) Nothing Nothing
        _ <- expectRight =<< runTestApp conn (Repo.Deck.listCardsOfDeck True cardQuery)

        afterRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        let beforeCount = fromOnly (head beforeRows)
        let afterCount = fromOnly (head afterRows)
        afterCount `shouldBe` beforeCount

  describe "listExplanationsOfDeck" $ do
    it "returns empty list when deck has no explanations" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "emptyexplainuser" "emptyexplain@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_empty_explain" :: String, "emptyexplainuser" :: String, "Empty Explanations" :: String, True, 0 :: Integer)
          return ()

        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Empty Explanations" "emptyexplainuser" "udv_empty_explain"))

        result <- runTestApp conn $ Repo.Deck.listExplanationsOfDeck (DeckContentQuery Nothing 10 (Models.Deck.deckId inserted) Nothing Nothing)
        pagedExplanations <- expectRight result
        let Explanation.PagedExplanations nextCursor items = pagedExplanations

        items `shouldBe` []
        nextCursor `shouldBe` Nothing

    it "limits explanation results and returns the next cursor" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "explainlimituser" "explainlimit@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_explain_limit" :: String, "explainlimituser" :: String, "Many Explanations" :: String, True, 0 :: Integer)
          return ()

        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Many Explanations" "explainlimituser" "udv_explain_limit"))

        _ <- runTestApp conn $ do
          mapM_ (\i -> execute
            "INSERT INTO user_explanation_views (id, user_id, user_deck_id, fen, move, text, visualizers) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("explain_" ++ show i, "explainlimituser" :: String, "udv_explain_limit" :: String, "fen " ++ show i, "e2e4" :: String, "Explain " ++ show i, "{}" :: String))
            [1..5 :: Int]

        result <- runTestApp conn $ Repo.Deck.listExplanationsOfDeck (DeckContentQuery Nothing 3 (Models.Deck.deckId inserted) Nothing Nothing)
        pagedExplanations <- expectRight result
        let Explanation.PagedExplanations nextCursor items = pagedExplanations

        length items `shouldBe` 3
        nextCursor `shouldSatisfy` isJust

    it "filters explanations by fen prefix" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "explainprefixuser" "explainprefix@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_explain_prefix" :: String, "explainprefixuser" :: String, "Prefix Explanations" :: String, True, 0 :: Integer)
          return ()

        inserted <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Prefix Explanations" "explainprefixuser" "udv_explain_prefix"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_explanation_views (id, user_id, user_deck_id, fen, move, text, visualizers) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("prefix_explain_1" :: String, "explainprefixuser" :: String, "udv_explain_prefix" :: String, "rnbqkbnr/pppppppp" :: String, "e2e4" :: String, "Opening explanation" :: String, "{}" :: String)
          _ <- execute "INSERT INTO user_explanation_views (id, user_id, user_deck_id, fen, move, text, visualizers) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("prefix_explain_2" :: String, "explainprefixuser" :: String, "udv_explain_prefix" :: String, "8/8/8/8" :: String, "d2d4" :: String, "Endgame explanation" :: String, "{}" :: String)
          return ()

        result <- runTestApp conn $ Repo.Deck.listExplanationsOfDeck (DeckContentQuery Nothing 10 (Models.Deck.deckId inserted) (Just "rnbqkbnr") Nothing)
        pagedExplanations <- expectRight result
        let Explanation.PagedExplanations _ items = pagedExplanations
        let Explanation.Explanation fen _ explanationText _ = head items

        length items `shouldBe` 1
        fen `shouldBe` "rnbqkbnr/pppppppp"
        explanationText `shouldBe` "Opening explanation"

    it "does not increment deck download_count" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "explaindownloaduser" "explaindownload@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert user

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
            ("udv_explain_dl" :: String, "explaindownloaduser" :: String, "Explanation Download" :: String, True, 0 :: Integer)
          return ()

        deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Explanation Download" "explaindownloaduser" "udv_explain_dl"))

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO user_explanation_views (id, user_id, user_deck_id, fen, move, text, visualizers) VALUES (?, ?, ?, ?, ?, ?, ?)"
            ("dl_explain_1" :: String, "explaindownloaduser" :: String, "udv_explain_dl" :: String, "fen" :: String, "e2e4" :: String, "Download explanation" :: String, "{}" :: String)
          return ()

        beforeRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        _ <- expectRight =<< runTestApp conn (Repo.Deck.listExplanationsOfDeck (DeckContentQuery Nothing 10 (Models.Deck.deckId deck) Nothing Nothing))

        afterRows <- query conn "SELECT download_count FROM decks WHERE id = ?" (Only $ Models.Deck.deckId deck) :: IO [Only Integer]

        fromOnly (head afterRows) `shouldBe` fromOnly (head beforeRows)

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

    it "returns DeckSearchResult data for matching decks" $ do
      withCleanDb $ \conn -> do
        let author = mkTestUser "searchcontmeta" "searchcontmeta@example.com" "password"
        let rater = mkTestUser "searchcontmetarater" "searchcontmetarater@example.com" "password"
        _ <- runTestApp conn $ Repo.User.insert author
        _ <- runTestApp conn $ Repo.User.insert rater

        insertedDeck <- insertDeckWithCards conn "searchcontmeta" "udv_cont_meta" "Continuation Metadata" True
          [ "e4 c5 Nf3"
          , "e4 c5 Nc3"
          ]

        _ <- runTestApp conn $ do
          _ <- execute "INSERT INTO deck_ratings (deck_id, user_id, rating) VALUES (?, ?, ?)"
            (Models.Deck.deckId insertedDeck, "searchcontmetarater" :: String, 4 :: Int)
          _ <- execute "UPDATE decks SET featured_source = ?, featured_card_id = ? WHERE id = ?"
            (Just ("curated" :: String), Just ("udv_cont_meta_card_2" :: String), Models.Deck.deckId insertedDeck)
          return ()

        result <- runTestApp conn $ Repo.Deck.searchContinuations "e4" Nothing Nothing Nothing
        response <- expectRight result

        length (decks response) `shouldBe` 1
        let resultDeck = (head (decks response)).deck
        resultDeck.name `shouldBe` "Continuation Metadata"
        resultDeck.previewMoves `shouldBe` "e4 c5"
        resultDeck.repertoire `shouldBe` "Both sides"
        resultDeck.featuredSource `shouldBe` Just "curated"
        resultDeck.featuredCardMoves `shouldBe` Just "e4 c5 Nc3"
        resultDeck.rating `shouldBe` Just 4.0
        resultDeck.ratingCount `shouldBe` 1
        resultDeck.downloadCount `shouldBe` 1
        (head (decks response)).deck_nr_cards `shouldBe` 2

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
