{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes #-}

module App.AppM where

import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Except
import Data.String (fromString)
import Servant (Handler, ServerError(..), err500)
import App.Env
import App.Error

-- | Application monad

newtype AppM a = AppM { runAppM :: ReaderT Env (ExceptT AppError Handler) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AppError)

instance ThrowAppError (AppM a) where
  throwAppError = throwError

nt :: Env -> AppM a -> Handler a
nt env (AppM m) = do
  r <- runExceptT (runReaderT m env)
  either (throwError . toServerError) pure r
