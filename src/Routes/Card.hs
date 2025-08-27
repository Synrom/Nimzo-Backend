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

module Routes.Card where

import Servant (type (:<|>) (..), ServerError(..), err404, err401, ReqBody, Post, JSON, type (:>), Capture, Patch, Get)
import Servant.Auth.Server (AuthResult(..), throwAll)
import Control.Monad.Reader
import Control.Monad.Except
import App.AppM
import App.Env
import App.Error
import App.Auth (AuthenticatedUser(..))
import Models.Card (Card(..))
import Models.User (User(..))
import Repo.Card
import Repo.Deck (authorsDeck)
import Repo.Utils

type API = 
  "deck" :> Capture "id" Integer :> "card" :> Get '[JSON] [Card]
  :<|> "deck" :> Capture "id" Integer :> "card" :> ReqBody '[JSON] Card :> Post '[JSON] Card
  :<|> "deck" :> Capture "id" Integer :> "card" :> ReqBody '[JSON] Card :> Patch '[JSON] Card
  

type Server = 
  (Integer -> AppM [Card])
  :<|> (Integer -> Card -> AppM Card)
  :<|> (Integer -> Card -> AppM Card)

associate :: Integer -> Card -> Card
associate deckid card = card {deckId = deckid}

notOwned :: AppError
notOwned = Unauthorized "You can only change decks you have authored."

ensureOwnership :: AuthenticatedUser -> Integer -> (Card -> AppM Card) -> Card -> AppM Card
ensureOwnership user deckid f card = do
  ensure notOwned =<< authorsDeck user.username deckid
  f card

prepare :: (Card -> AppM Card) -> AuthenticatedUser -> Integer -> Card -> AppM Card
prepare route user deckid card = ensureOwnership user deckid route (associate deckid card)

server :: AuthResult AuthenticatedUser -> Server
server (Authenticated user) = 
  Repo.Card.find
  :<|> prepare Repo.Card.insert user
  :<|> prepare Repo.Card.update user
server _ = throwAppError $ Unauthorized "No access."
