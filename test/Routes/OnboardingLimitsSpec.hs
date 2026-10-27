{-# LANGUAGE OverloadedStrings #-}
module Routes.OnboardingLimitsSpec (spec) where
import App.OnboardingLimits
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Hspec

spec :: Spec
spec = describe "Onboarding public request limits" $ do
 it "limits the revenuecat webhook per peer without affecting other routes" $ do
  limits <- newOnboardingLimits
  let app = limits $ \_ respond -> respond $ responseLBS status200 [] "ok"
      req path = defaultRequest {rawPathInfo=path}
  responses <- runSession (replicateM 601 (request $ req "/onboarding/revenuecat")) app
  simpleStatus (last responses) `shouldBe` status429
  lookup "Retry-After" (simpleHeaders $ last responses) `shouldBe` Just "60"
  events <- runSession (request $ req "/onboarding/events") app
  simpleStatus events `shouldBe` status200
  config <- runSession (request $ req "/onboarding/config") app
  simpleStatus config `shouldBe` status200
  progress <- runSession (request $ req "/onboarding/anonymous/progress") app
  simpleStatus progress `shouldBe` status200
 it "rejects oversized bodies before the downstream JSON parser runs" $ do
  limits <- newOnboardingLimits
  let app = limits $ \_ respond -> respond $ responseLBS status200 [] "parser-ran"
      req = defaultRequest {rawPathInfo="/onboarding/revenuecat",requestMethod="POST",requestBodyLength=ChunkedBody}
  response <- runSession (srequest $ SRequest req $ BL.replicate 262145 32) app
  simpleStatus response `shouldBe` status413
 it "passes valid webhook bodies intact" $ do
  limits <- newOnboardingLimits
  let app = limits $ \req respond -> do
        body <- strictRequestBody req
        respond $ responseLBS status200 [] body
  response <- runSession (srequest $ SRequest defaultRequest {rawPathInfo="/onboarding/revenuecat"} "{\"event\":{}}") app
  simpleBody response `shouldBe` "{\"event\":{}}"
