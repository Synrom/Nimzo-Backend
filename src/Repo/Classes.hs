{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Repo.Classes where

import Data.Int
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception
import Servant
import Database.PostgreSQL.Simple as Postgres
import Network.Mail.Mime (Mail)
import Servant.Auth.Server (JWTSettings)
import App.Env
import App.AppM
import App.Config
import App.Error (AppError, fromSqlError)

type MonadApp m = (MonadError AppError m, MonadIO m)

class MonadApp m => MonadDB m where
  askEnv :: m Env
  runQuery :: (ToRow q, FromRow r) => Query -> q -> m [r]
  execute :: ToRow q => Query -> q -> m Int64

runIOorThrow :: (MonadIO m, MonadError AppError m) => IO a -> m a
runIOorThrow io = do
  r <- liftIO $ try @SqlError io
  either (throwError . fromSqlError) pure r

instance MonadDB AppM where
  askEnv = ask
  runQuery q ps = do
    env <- ask
    runIOorThrow $ withConn env (\c -> Postgres.query c q ps)
  execute q ps = do
    env <- ask
    runIOorThrow $ withConn env (\c -> Postgres.execute c q ps)

class MonadApp m => MonadMail m where
  sendMail :: Mail -> m ()
  mailCfg :: m MailConfiguration

class MonadApp m => MonadJWTSettings m where
  askJwtCfg :: m JWTSettings

instance MonadJWTSettings AppM where
  askJwtCfg = do
    env <- askEnv
    return env.jwtSettings