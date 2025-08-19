{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Watermelon where

import Data.Text
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Data.Proxy
import Database (Table(..))
import Data.Int (Int64)
import Control.Monad (filterM)
import Servant (Handler)
import Control.Exception (try)


data TableChanges a = TableChanges {
  created :: [a],
  updated :: [a],
  deleted :: [String]
}

-- data Changes = Changes {
--   users :: TableChanges User
-- }
-- 
-- deleteM :: (String -> IO Int64) -> [String] -> IO [String]
-- deleteM deleteFn = filterM $ fmap (/= 0) . deleteFn
-- 
-- mapE :: (a -> IO b) -> [a] -> IO [Either SqlError b]
-- mapE f = mapM $ try . f
-- 
-- executeTableChanges :: Table a => TableChanges a -> Connection -> Handler (TableChanges a)
-- executeTableChanges (TableChanges created updated deleted) conn = do
--   newelms <- mapM (insert conn) created
--   changedelms <- mapM (change conn) updated
--   deletedelms <- deleteM (delete conn newelms) deleted
--   return $ TableChanges newelms changedelms deletedelms 
-- 
-- executeChanges :: Changes -> Connection -> Handler Changes
-- executeChanges (Changes users) conn = do
--   userchanges <- executeTableChanges users conn
--   return $ Changes userchanges