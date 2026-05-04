{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Repo.Classes where

import Data.Int
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception
import Servant
import qualified Database.PostgreSQL.Simple as Postgres
import Network.Mail.Mime (Mail)
import Servant.Auth.Server (JWTSettings)
import App.Env
import App.AppM
import App.Config
import App.Error (AppError, fromSqlError)

type MonadApp m = (MonadError AppError m, MonadIO m)

class MonadApp m => MonadDB m where
  askEnv :: m Env
  runQuery :: (Postgres.ToRow q, Postgres.FromRow r) => Postgres.Query -> q -> m [r]
  execute :: Postgres.ToRow q => Postgres.Query -> q -> m Int64
  withTransaction :: m a -> m a

runIOorThrow :: (MonadIO m, MonadError AppError m) => IO a -> m a
runIOorThrow io = do
  r <- liftIO $ try @Postgres.SqlError io
  either (liftIO . fromSqlError >=> throwError) pure r

instance MonadDB AppM where
  askEnv = ask
  runQuery q ps = do
    env <- ask
    runIOorThrow $ withConn env (\c -> Postgres.query c q ps)
  execute q ps = do
    env <- ask
    runIOorThrow $ withConn env (\c -> Postgres.execute c q ps)
  withTransaction action = do
    _ <- execute "BEGIN" ()
    result <- action `catchError` \err -> do
      _ <- execute "ROLLBACK" ()
      throwError err
    _ <- execute "COMMIT" ()
    pure result

class MonadApp m => MonadMail m where
  sendMail :: Mail -> m ()
  mailCfg :: m MailConfiguration

class MonadApp m => MonadJWTSettings m where
  askJwtCfg :: m JWTSettings

instance MonadJWTSettings AppM where
  askJwtCfg = do
    env <- askEnv
    return env.jwtSettings
