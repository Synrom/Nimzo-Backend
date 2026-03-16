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
import Control.Monad.IO.Class (liftIO)
import Servant (Handler, throwError)
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
  either
    (\appErr -> do
      liftIO $ logAppError appErr
      throwError $ toServerError appErr)
    pure
    r
