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

type API = 
  "deck" :> ReqBody '[JSON] Deck :> Post '[JSON] Deck
  :<|> "deck" :> "search" :> QueryParam "query" String :> Get '[JSON] [Deck]
  :<|> "deck" :> Capture "id" Integer :> ReqBody '[JSON] Deck :> Patch '[JSON] Deck
  :<|> "deck" :> Capture "id" Integer :> Get '[JSON] Deck

type Server = 
  (Deck -> AppM Deck)
  :<|> (Maybe String -> AppM [Deck])
  :<|> (Integer -> Deck -> AppM Deck)
  :<|> (Integer -> AppM Deck)

own :: AuthenticatedUser -> Deck -> Deck
own user deck = deck {author = user.username}

setId :: Integer -> Deck -> Deck
setId id deck = deck { deckId = id }

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = 
  Repo.Deck.insert . own user
  :<|> Repo.Deck.search
  :<|> (Repo.Deck.update.) . (own user.) . setId 
  :<|> Repo.Deck.find
server _ = throwAppError $ Unauthorized "No access."