module Routes.AndroidSpec (spec) where

import Test.Hspec

import TestHelpers
import Models.AndroidEmail (AndroidEmailPayload(..))
import qualified Models.AndroidEmail as Models
import Models.Watermelon (JsonableMsg(..))
import qualified Repo.AndroidEmail as Repo.AndroidEmail
import qualified Routes.Android as Routes.Android
import App.Error (AppError(..))

spec :: Spec
spec = describe "Routes.Android" $ do
  describe "saveAndroidEmail" $ do
    it "saves the provided email" $ do
      withCleanDb $ \conn -> do
        let payload = AndroidEmailPayload "android@example.com"
        result <- runTestApp conn $ Routes.Android.saveAndroidEmail payload
        Msg msg <- expectRight result
        msg `shouldBe` "Successfully saved android email."

        inserted <- runTestApp conn $ Repo.AndroidEmail.findByEmail "android@example.com"
        saved <- expectRight inserted >>= expectJust
        case saved of
          Models.AndroidEmail _ savedEmail -> savedEmail `shouldBe` "android@example.com"

    it "rejects empty emails" $ do
      withCleanDb $ \conn -> do
        result <- runTestApp conn $ Routes.Android.saveAndroidEmail (AndroidEmailPayload "   ")
        err <- expectLeft result
        case err of
          Internal message -> message `shouldBe` "{\"msg\":\"Invalid email.\"}"
          _ -> expectationFailure $ "Expected Internal test wrapper error but got: " ++ show err
