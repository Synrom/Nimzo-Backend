{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

-- Import all test modules
import qualified Repo.UserSpec
import qualified Repo.DeckSpec
import qualified Repo.OnboardingSpec
import qualified Routes.AndroidSpec
import qualified Routes.AuthSpec
import qualified Routes.DeckSpec
import qualified Routes.ExperimentSpec
import qualified Routes.UserSpec
import qualified Routes.WatermelonSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Repository Tests" $ do
    describe "User Repository" Repo.UserSpec.spec
    describe "Deck Repository" Repo.DeckSpec.spec
    describe "Onboarding Repository" Repo.OnboardingSpec.spec

  describe "Route Tests" $ do
    describe "Android Routes" Routes.AndroidSpec.spec
    describe "Auth Routes" Routes.AuthSpec.spec
    describe "Deck Routes" Routes.DeckSpec.spec
    describe "Experiment Routes" Routes.ExperimentSpec.spec
    describe "User Routes" Routes.UserSpec.spec
    describe "Watermelon Sync Routes" Routes.WatermelonSpec.spec
