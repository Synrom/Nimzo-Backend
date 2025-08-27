{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Repo.Utils where

import Control.Monad
import Control.Monad.Except
import App.Error

notFound :: AppError
notFound = NotFound "Could not find item."

one :: MonadError AppError m => [a] -> m a
one = \case
  [x] -> return x
  _   -> throwError notFound

orThrow :: MonadError e m => e -> Maybe a -> m a
orThrow e = maybe (throwError e) pure

rightOrThrow :: MonadError e m => e -> Either a b -> m b
rightOrThrow e i = case i of
  Right c ->  pure c
  _ -> throwError e

ensure :: MonadError e m => e -> Bool -> m ()
ensure e ok = unless ok (throwError e)

notNull :: Foldable t => t a -> Bool
notNull = not . null

flattenChangeset :: (c -> [a]) -> (c -> [a]) -> c -> [a]
flattenChangeset getCreated getUpdated c = getCreated c ++ getUpdated c

orM :: Monad m => [m Bool] -> m Bool
orM as = or <$> sequence as

neitherM :: Monad m => [m Bool] -> m Bool
neitherM as = not <$> orM as

ensureM :: (Monad m, MonadError e m) => e -> m Bool -> m ()
ensureM err mcond = do
  ok <- mcond
  if ok then pure () else throwError err