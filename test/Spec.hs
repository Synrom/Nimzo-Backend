module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Trivial" $ do
    it "True work" $ do 
      1 `shouldBe` 1