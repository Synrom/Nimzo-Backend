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
import Servant (type (:<|>) (..), ServerError(..), err404, err401, type (:>), ReqBody, JSON, QueryParam, Post, Get, Patch, Capture)
import Servant.Auth.Server (AuthResult(..), throwAll)
import Control.Monad.Reader
import Control.Monad.Except
import App.AppM
import App.Env
import App.Error
import App.Auth (AuthenticatedUser(..))
import Models.Deck (Deck(..))
import Models.User (User(..))
import Repo.Deck
import Models.Card (Card, CardQuery, PagedCards)

type API =
  "deck" :> "search" :> "full" :> QueryParam "query" String :> Get '[JSON] [Deck]
  :<|> "deck" :> "search" :> "instant" :> QueryParam "query" String :> Get '[JSON] [Deck]
  :<|> "deck" :> Capture "id" Integer :> Get '[JSON] Deck
  :<|> "deck" :> "cards" :> ReqBody '[JSON] CardQuery :> Post '[JSON] PagedCards
  :<|> "deck" :> Capture "user_deck_id" String :> "continuations" :> QueryParam "prefix" String :> Get '[JSON] [String]

type Server =
  (Maybe String -> AppM [Deck])
  :<|> (Maybe String -> AppM [Deck])
  :<|> (Integer -> AppM Deck)
  :<|> (CardQuery -> AppM PagedCards)
  :<|> (String -> Maybe String -> AppM [String])

own :: AuthenticatedUser -> Deck -> Deck
own user deck = deck {author = user.username}

setId :: Integer -> Deck -> Deck
setId id deck = deck { deckId = id }

server :: Server
server =
  Repo.Deck.search
  :<|> Repo.Deck.searchInstant
  :<|> Repo.Deck.find
  :<|> Repo.Deck.listCardsOfDeck
  :<|> (\deckId mPrefix -> Repo.Deck.listContinuations deckId (fromMaybe "" mPrefix))