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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module App.Server where

import Servant (Server, ServerT, hoistServerWithContext, type (:<|>) (..))
import Servant.Auth.Server
import Data.Proxy
import App.API
import App.AppM
import App.Env
import qualified Routes.Deck as DeckRoutes
import qualified Routes.Auth as AuthRoutes
import qualified Routes.Watermelon as Watermelon
import qualified Routes.User as User
import App.Auth (AuthenticatedUser)

secureServerT :: AuthResult AuthenticatedUser -> ServerT SecureAPI AppM
secureServerT auth = DeckRoutes.server auth 
  :<|> Watermelon.server auth 
  :<|> User.server auth 

serverT :: ServerT API AppM
serverT = secureServerT :<|> AuthRoutes.server

mkServer :: Env -> Servant.Server API
mkServer env = hoistServerWithContext api (Proxy :: Proxy '[JWTSettings, CookieSettings]) (nt env) serverT