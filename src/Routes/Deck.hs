{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Routes.Deck where

import Data.Proxy
import Data.Maybe (fromMaybe)
import Servant (type (:<|>) (..), type (:>), ReqBody, JSON, QueryParam, Post, Get, Capture)
import Servant.Auth.Server (AuthResult(..), throwAll)
import App.AppM
import App.Error (throwAppError, AppError(..))
import App.Auth (AuthenticatedUser(..))
import Models.DeckDetails (DeckDetails(..))
import Models.DeckSearch (SearchContinuationsResponse, DeckSearchResult)
import Models.DeckRating (DeckRatingRequest(..))
import Models.Watermelon (JsonableMsg)
import Repo.Deck
import Models.Card (Card, CardQuery, PagedCards)

type API =
  "deck" :> "search" :> "full" :> QueryParam "query" String :> Get '[JSON] [DeckSearchResult]
  :<|> "deck" :> "search" :> "instant" :> QueryParam "query" String :> Get '[JSON] [DeckSearchResult]
  :<|> "deck" :> "search" :> "continuations" :> QueryParam "prefix" String :> QueryParam "color" String :> QueryParam "limitDecks" Integer :> QueryParam "limitContinuations" Integer :> Get '[JSON] SearchContinuationsResponse
  :<|> "deck" :> "cards" :> ReqBody '[JSON] CardQuery :> Post '[JSON] PagedCards
  :<|> "deck" :> Capture "user_deck_id" String :> "continuations" :> QueryParam "prefix" String :> Get '[JSON] [String]

type SecureAPI =
  "deck" :> Capture "id" Integer :> Get '[JSON] DeckDetails
  :<|> "deck" :> Capture "id" Integer :> "rating" :> ReqBody '[JSON] DeckRatingRequest :> Post '[JSON] JsonableMsg

type Server =
  (Maybe String -> AppM [DeckSearchResult])
  :<|> (Maybe String -> AppM [DeckSearchResult])
  :<|> (Maybe String -> Maybe String -> Maybe Integer -> Maybe Integer -> AppM SearchContinuationsResponse)
  :<|> (CardQuery -> AppM PagedCards)
  :<|> (String -> Maybe String -> AppM [String])

type SecureServer =
  (Integer -> AppM DeckDetails)
  :<|> (Integer -> DeckRatingRequest -> AppM JsonableMsg)

server :: Server
server =
  Repo.Deck.search
  :<|> Repo.Deck.searchInstant
  :<|> (Repo.Deck.searchContinuations . fromMaybe "")
  :<|> Repo.Deck.listCardsOfDeck
  :<|> (\deckId mPrefix -> Repo.Deck.listContinuations deckId (fromMaybe "" mPrefix))

secureServer :: AuthResult AuthenticatedUser -> SecureServer
secureServer auth =
  (\deckId -> Repo.Deck.findWithRating (fmap (.username) authenticatedUser) deckId)
  :<|> ratingHandler
  where
    authenticatedUser :: Maybe AuthenticatedUser
    authenticatedUser = case auth of
      Authenticated user -> Just user
      _ -> Nothing

    ratingHandler :: Integer -> DeckRatingRequest -> AppM JsonableMsg
    ratingHandler = case authenticatedUser of
      Just user -> \deckId payload -> Repo.Deck.saveRating user.username deckId payload.rating
      Nothing -> \_ _ -> throwAppError $ Unauthorized "No access."
