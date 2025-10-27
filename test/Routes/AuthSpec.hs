{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Routes.AuthSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Data.Time (getCurrentTime, addUTCTime, secondsToNominalDiffTime)
import Control.Monad.IO.Class

import TestHelpers
import Routes.Auth
import Routes.User
import Repo.User
import Models.User
import App.Auth
import Repo.Classes (MonadMail(mailCfg))
import App.Config 

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = describe "Routes.Auth" $ do

  describe "createUser" $ do
    it "creates a new user with hashed password" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "newuser" "newuser@example.com" "plainpassword"

        result <- runTestApp conn mailCfg
        mail_cfg <- expectRight result
        mail_cfg.test `shouldBe` True

        result <- runTestApp conn $ Routes.Auth.createUser user
        newUserData <- expectRight result

        newUserData.username `shouldBe` "newuser"
        newUserData.email `shouldBe` "newuser@example.com"
        newUserData.xp `shouldBe` 0
        newUserData.verified `shouldBe` False

        -- Check that tokens were created
        newUserData.auth.access_token `shouldSatisfy` (not . null)
        newUserData.auth.refresh_token `shouldSatisfy` (not . null)

    it "stores the user with a hashed password, not plaintext" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "hashtest" "hash@example.com" "mypassword"

        _ <- runTestApp conn $ Routes.Auth.createUser user

        -- Fetch the user from database
        dbUserResult <- runTestApp conn $ Repo.User.find "hashtest"
        Just dbUser <- expectRight dbUserResult

        -- Password should not match plaintext
        Models.User.password dbUser `shouldNotBe` T.pack "mypassword"
        -- Password should be hashed
        T.length (Models.User.password dbUser) `shouldSatisfy` (> 20)

    it "fails when username already exists" $ do
      withCleanDb $ \conn -> do
        let user1 = mkTestUser "duplicate" "user1@example.com" "password1"
        let user2 = mkTestUser "duplicate" "user2@example.com" "password2"

        _ <- runTestApp conn $ Routes.Auth.createUser user1
        result <- runTestApp conn $ Routes.Auth.createUser user2

        result `shouldSatisfy` isLeft'

  describe "authCheck" $ do
    it "successfully authenticates with correct credentials" $ do
      withCleanDb $ \conn -> do
        -- Create a user first
        let password = "correctpassword"
        let user = mkTestUser "authuser" "auth@example.com" password
        _ <- runTestApp conn $ Routes.Auth.createUser user

        -- Try to authenticate
        let authReq = AuthRequest "authuser" password
        result <- runTestApp conn $ Routes.Auth.authCheck authReq
        authData <- expectRight result

        authData.username `shouldBe` "authuser"
        authData.auth.access_token `shouldSatisfy` (not . null)

    it "authenticates with email instead of username" $ do
      withCleanDb $ \conn -> do
        let password = "emailpassword"
        let user = mkTestUser "emailauth" "emailauth@example.com" password
        _ <- runTestApp conn $ Routes.Auth.createUser user

        let authReq = AuthRequest "emailauth@example.com" password
        result <- runTestApp conn $ Routes.Auth.authCheck authReq
        authData <- expectRight result

        authData.username `shouldBe` "emailauth"

    it "fails with incorrect password" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "wrongpass" "wrongpass@example.com" "correctpassword"
        _ <- runTestApp conn $ Routes.Auth.createUser user

        let authReq = AuthRequest "wrongpass" "wrongpassword"
        result <- runTestApp conn $ Routes.Auth.authCheck authReq

        result `shouldSatisfy` isLeft'

    it "fails with non-existent username" $ do
      withCleanDb $ \conn -> do
        let authReq = AuthRequest "nonexistent" "anypassword"
        result <- runTestApp conn $ Routes.Auth.authCheck authReq

        result `shouldSatisfy` isLeft'

  describe "refreshToken" $ do
    it "successfully refreshes access token with valid refresh token" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "refreshuser" "refresh@example.com" "password"
        result <- runTestApp conn $ do
          newUserData <- Routes.Auth.createUser user
          let refreshReq = AuthTokenRequest newUserData.auth.refresh_token
          tokens <- Routes.Auth.refreshToken refreshReq
          liftIO $ tokens.refresh_token `shouldBe` newUserData.auth.refresh_token
          return tokens

        tokens <- expectRight result
        tokens.access_token `shouldSatisfy` (not . null)

    it "fails with invalid refresh token" $ do
      withCleanDb $ \conn -> do
        let refreshReq = AuthTokenRequest "invalid.token.here"
        result <- runTestApp conn $ Routes.Auth.refreshToken refreshReq

        result `shouldSatisfy` isLeft'

  describe "password hashing and salt generation" $ do
    it "generates different salts each time" $ do
      salt1 <- generateSalt
      salt2 <- generateSalt

      salt1 `shouldNotBe` salt2
      T.length salt1 `shouldBe` 16
      T.length salt2 `shouldBe` 16

    it "same password with different salts produces different hashes" $ do
      salt1 <- generateSalt
      salt2 <- generateSalt
      let password = T.pack "testpassword"

      let hash1 = hashWithSalt salt1 password
      let hash2 = hashWithSalt salt2 password

      hash1 `shouldNotBe` hash2

    it "same password with same salt produces same hash" $ do
      let salt = T.pack "fixedsalt1234567"
      let password = T.pack "testpassword"

      let hash1 = hashWithSalt salt password
      let hash2 = hashWithSalt salt password

      hash1 `shouldBe` hash2

  describe "verify user" $ do
    it "checks that verifying a user works" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "xpuser" "xp@example.com" "hashedpassword"

        newUserData <- expectRight =<< runTestApp conn (Routes.Auth.createUser user)
        userBeforeVerification <- expectRight =<< runTestApp conn (Routes.User.getUserRoute user.username)
        userBeforeVerification.verified `shouldBe` False

        result <- runTestApp conn $ do
          token <- createUserVerification user
          Routes.Auth.verifyUser $ Token token
        _ <- expectRight result

        userAfterVerification <- expectRight =<< runTestApp conn (Routes.User.getUserRoute user.username)
        userAfterVerification.verified `shouldBe` True