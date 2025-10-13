{-# LANGUAGE OverloadedRecordDot #-}

module Repo.Xp where
import Data.Time (UTCTime, getCurrentTime)
import Repo.Utils 
import Models.User (User(..))

streakMultiplier :: Integer -> Float
streakMultiplier streak
  | streak <= 6  = 1.0
  | streak <= 13 = 1.2
  | streak <= 29 = 1.5
  | streak <= 59 = 2.0
  | otherwise    = 2.5

multiplyAndRoundUp :: Integer -> Float -> Integer
multiplyAndRoundUp x y = ceiling (fromInteger x * y)

calcXp :: Integer -> Integer -> Integer
calcXp nrCardsReviewed streak = multiplyAndRoundUp points $ streakMultiplier streak
  where
    points = nrCardsReviewed * 10
