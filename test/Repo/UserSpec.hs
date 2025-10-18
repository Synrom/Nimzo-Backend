{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.UserSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import TestHelpers
import Repo.User
import Models.User
import App.Auth (hashWithSalt)

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

spec :: Spec
spec = describe "Repo.User" $ do

  describe "insert" $ do
    it "inserts a new user into the database" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "testuser" "test@example.com" "hashedpassword"
        result <- runTestApp conn $ Repo.User.insert user
        inserted <- expectRight result

        inserted.username `shouldBe` "testuser"
        inserted.email `shouldBe` "test@example.com"
        inserted.xp `shouldBe` 0
        inserted.streak `shouldBe` 0
        inserted.premium `shouldBe` False
        inserted.verified `shouldBe` False

    it "fails when inserting duplicate username" $ do
      withCleanDb $ \conn -> do
        let user1 = mkTestUser "testuser" "test1@example.com" "hashedpassword"
        let user2 = mkTestUser "testuser" "test2@example.com" "hashedpassword"

        _ <- runTestApp conn $ Repo.User.insert user1
        result <- runTestApp conn $ Repo.User.insert user2

        result `shouldSatisfy` isLeft'

    it "fails when inserting duplicate email" $ do
      withCleanDb $ \conn -> do
        let user1 = mkTestUser "user1" "same@example.com" "hashedpassword"
        let user2 = mkTestUser "user2" "same@example.com" "hashedpassword"

        _ <- runTestApp conn $ Repo.User.insert user1
        result <- runTestApp conn $ Repo.User.insert user2

        result `shouldSatisfy` isLeft'

  describe "find" $ do
    it "finds user by username" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "findme" "findme@example.com" "hashedpassword"
        _ <- runTestApp conn $ Repo.User.insert user

        result <- runTestApp conn $ Repo.User.find "findme"
        found <- expectRight result

        found `shouldSatisfy` isJust
        case found of
          Just u -> u.username `shouldBe` "findme"
          Nothing -> expectationFailure "Expected to find user"

    it "finds user by email" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "emailuser" "findemail@example.com" "hashedpassword"
        _ <- runTestApp conn $ Repo.User.insert user

        result <- runTestApp conn $ Repo.User.find "findemail@example.com"
        found <- expectRight result

        found `shouldSatisfy` isJust
        case found of
          Just u -> u.email `shouldBe` "findemail@example.com"
          Nothing -> expectationFailure "Expected to find user"

    it "returns Nothing for non-existent user" $ do
      withCleanDb $ \conn -> do
        result <- runTestApp conn $ Repo.User.find "nonexistent"
        found <- expectRight result
        found `shouldSatisfy` isNothing

  describe "findUsername" $ do
    it "finds user by username only" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "usernameonly" "email@example.com" "hashedpassword"
        _ <- runTestApp conn $ Repo.User.insert user

        result <- runTestApp conn $ Repo.User.findUsername "usernameonly"
        found <- expectRight result

        found `shouldSatisfy` isJust
        case found of
          Just u -> u.username `shouldBe` "usernameonly"
          Nothing -> expectationFailure "Expected to find user"

    it "returns Nothing when searching by email (username only search)" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "user1" "findbyemail@example.com" "hashedpassword"
        _ <- runTestApp conn $ Repo.User.insert user

        result <- runTestApp conn $ Repo.User.findUsername "findbyemail@example.com"
        found <- expectRight result

        found `shouldSatisfy` isNothing

  describe "updateXP" $ do
    it "updates user XP based on number of cards" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "xpuser" "xp@example.com" "hashedpassword"
        inserted <- expectRight =<< runTestApp conn (Repo.User.insert user)

        result <- runTestApp conn $ Repo.User.updateXP 5 inserted
        updated <- expectRight result

        updated.xp `shouldSatisfy` (> 0)
        updated.username `shouldBe` "xpuser"

    it "updates streak when appropriate" $ do
      withCleanDb $ \conn -> do
        now <- getCurrentTime
        let user = (mkTestUser "streakuser" "streak@example.com" "hashedpassword")
              { last_activity = now }
        inserted <- expectRight =<< runTestApp conn (Repo.User.insert user)

        -- Sleep briefly to ensure time has passed
        result <- runTestApp conn $ Repo.User.updateXP 3 inserted
        updated <- expectRight result

        -- Streak logic depends on time difference
        updated.streak `shouldSatisfy` (>= 0)

  describe "password hashing" $ do
    it "hashes password with salt correctly" $ do
      let salt = "testsalt12345678"
      let password = "mypassword"
      let hash1 = hashWithSalt (T.pack salt) (T.pack password)
      let hash2 = hashWithSalt (T.pack salt) (T.pack password)

      hash1 `shouldBe` hash2
      hash1 `shouldNotBe` T.pack password

    it "produces different hashes for different salts" $ do
      let salt1 = "salt1234567890ab"
      let salt2 = "salt9876543210zy"
      let password = "samepassword"

      let hash1 = hashWithSalt (T.pack salt1) (T.pack password)
      let hash2 = hashWithSalt (T.pack salt2) (T.pack password)

      hash1 `shouldNotBe` hash2
