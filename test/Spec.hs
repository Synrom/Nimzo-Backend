{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

-- Import all test modules
import qualified Repo.UserSpec
import qualified Repo.DeckSpec
import qualified Routes.AuthSpec
import qualified Routes.WatermelonSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Repository Tests" $ do
    describe "User Repository" Repo.UserSpec.spec
    describe "Deck Repository" Repo.DeckSpec.spec

  describe "Route Tests" $ do
    describe "Auth Routes" Routes.AuthSpec.spec
    describe "Watermelon Sync Routes" Routes.WatermelonSpec.spec