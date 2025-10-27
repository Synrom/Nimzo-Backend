{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module App.API where

import Data.Proxy
import Servant
  ( Capture,
    Context (EmptyContext),
    Get,
    Handler,
    JSON,
    Patch,
    Post,
    QueryParam,
    ReqBody,
    Server,
    ServerError,
    err401,
    err404,
    err500,
    serveWithContext,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Auth.Server
  ( AuthResult (Authenticated),
    FromJWT,
    JWTSettings,
    ToJWT,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
    makeJWT,
    throwAll,
    verifyJWT,
  )
import Servant.Server (Context ((:.)))
import Data.Text
import Servant.Foreign
import Servant.Auth (Auth, JWT)
import Models.User (User(..))
import Models.Deck (Deck(..))
import Models.UserDeckView (UserDeckView(..))
import Models.UserCardView (UserCardView(..))
import App.Auth (AuthenticatedUser(..), AuthRequest(..), AuthTokens(..), NewUser(..))
import qualified Routes.Deck as DeckRoutes
import qualified Routes.Auth as AuthRoutes
import qualified Routes.Watermelon as Watermelon
import qualified Routes.User as User

type SecureAPI =
  DeckRoutes.API 
  :<|> Watermelon.API
  :<|> User.API

type API =
  Auth '[JWT] AuthenticatedUser :> SecureAPI
    :<|> AuthRoutes.API

api :: Proxy API
api = Proxy

instance (HasForeign lang ftype api) 
  => HasForeign lang ftype (Auth authSchemes user :> api) where
  type Foreign ftype (Auth authSchemes user :> api) = Foreign ftype api
  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

endpoints :: [Req NoContent]
endpoints = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) (Proxy :: Proxy API)