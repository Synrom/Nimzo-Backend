{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.DeckSpec (spec) where

import Test.Hspec
import Database.PostgreSQL.Simple (Connection)

import qualified Models.Card as Card
import qualified Models.Deck
import Models.Card (DeckContentQuery(..))
import Repo.Classes (execute)
import qualified Repo.Deck
import qualified Repo.User
import qualified Routes.Deck
import TestHelpers

spec :: Spec
spec = describe "Routes.Deck" $ do
  describe "listCardsHandler" $ do
    it "defaults missing schemaVersion to 3 and excludes fen cards" $ do
      withCleanDb $ \conn -> do
        deck <- seedDeckWithFenAndMovesCards conn "deckcardsdefault" "udv_deck_cards_default"

        let cardQuery = DeckContentQuery Nothing 10 (Models.Deck.deckId deck) Nothing Nothing
        pagedCards <- expectRight =<< runTestApp conn (Routes.Deck.listCardsHandler cardQuery)

        map (.title) (Card.cards pagedCards) `shouldBe` ["Moves card"]
        map (.fen) (Card.cards pagedCards) `shouldBe` [Nothing]

    it "includes fen cards for schemaVersion 4" $ do
      withCleanDb $ \conn -> do
        deck <- seedDeckWithFenAndMovesCards conn "deckcardsv4" "udv_deck_cards_v4"

        let cardQuery = DeckContentQuery Nothing 10 (Models.Deck.deckId deck) Nothing (Just 4)
        pagedCards <- expectRight =<< runTestApp conn (Routes.Deck.listCardsHandler cardQuery)

        map (.title) (Card.cards pagedCards) `shouldBe` ["Fen card", "Moves card"]
        map (.fen) (Card.cards pagedCards) `shouldBe` [Just "8/8/8/8/8/8/8/8 w - - 0 1", Nothing]

seedDeckWithFenAndMovesCards :: Connection -> String -> String -> IO Models.Deck.Deck
seedDeckWithFenAndMovesCards conn username userDeckId = do
  let user = mkTestUser username (username ++ "@example.com") "password"
  _ <- runTestApp conn $ Repo.User.insert user

  _ <- runTestApp conn $ do
    _ <- execute "INSERT INTO user_deck_views (id, user_id, name, is_public, num_cards_total) VALUES (?, ?, ?, ?, ?)"
      (userDeckId, username, "Deck Cards" :: String, True, 2 :: Integer)
    pure ()

  deck <- expectRight =<< runTestApp conn (Repo.Deck.insertOrUpdate (mkTestDeck 0 "Deck Cards" username userDeckId))

  _ <- runTestApp conn $ do
    _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, fen, next_request) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      ("card_fen_" ++ username, username, userDeckId, "e4" :: String, "Fen card" :: String, "wh" :: String, Just ("8/8/8/8/8/8/8/8 w - - 0 1" :: String), 0 :: Integer)
    _ <- execute "INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request) VALUES (?, ?, ?, ?, ?, ?, ?)"
      ("card_moves_" ++ username, username, userDeckId, "d4 d5" :: String, "Moves card" :: String, "wh" :: String, 0 :: Integer)
    pure ()

  pure deck
