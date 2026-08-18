{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.StreakNotificationSpec (spec) where

import Data.Aeson (Value(..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Test.Hspec
import App.Error (AppError(..))
import App.APNS (APNSRequest(..), APNSResponse(..))
import Models.StreakNotification
import qualified Repo.StreakNotification as Notifications
import qualified Repo.User as Users
import TestHelpers
import Worker.StreakNotification

closeTo :: NominalDiffTime -> NominalDiffTime -> Bool
closeTo expected actual = abs (actual - expected) < 5

spec :: Spec
spec = do
  describe "ActivityKit token validation" $ do
    it "accepts uppercase hex and rejects malformed or oversized values" $ do
      Notifications.decodeActivityToken "0AFF" `shouldSatisfy` either (const False) (const True)
      Notifications.decodeActivityToken "" `shouldSatisfy` isLeft
      Notifications.decodeActivityToken "abc" `shouldSatisfy` isLeft
      Notifications.decodeActivityToken "zz" `shouldSatisfy` isLeft
      Notifications.decodeActivityToken (T.replicate 1026 "a") `shouldSatisfy` isLeft

  describe "ActivityKit payloads" $ do
    let now = read "2026-08-15 14:12:31 UTC"
        starts = now
        completes = addUTCTime 3600 now
    it "uses Swift field names and Unix seconds for start" $ do
      let json = LBS8.unpack $ encode $ startPayload now "schedule-id" 14 starts completes True
      json `shouldContain` "\"content-state\""
      json `shouldContain` "\"endsAtTimestamp\":1786806751"
      json `shouldContain` "\"attributes-type\":\"StreakActivityAttributes\""
      json `shouldContain` "\"scheduleId\":\"schedule-id\""
      json `shouldContain` "\"startsAtTimestamp\":1786803151"
      json `shouldContain` "\"input-push-token\":1"
    it "uses update and end events with a final complete state" $ do
      LBS8.unpack (encode $ completePayload now completes) `shouldContain` "\"event\":\"update\""
      LBS8.unpack (encode $ completePayload now completes) `shouldContain` "\"isComplete\":true"
      let final = LBS8.unpack $ encode $ endPayload now completes True
      final `shouldContain` "\"event\":\"end\""
      final `shouldContain` "\"isComplete\":true"
      final `shouldContain` "\"dismissal-date\""

  describe "notification repository" $ do
    it "is idempotent, enforces ownership, and replaces schedules atomically" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "streak-a" "a@example.test" "password")
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "streak-b" "b@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.6" "1.4.0" "82" True Production
          startToken = PushToStartTokenRequest "A0ff" "StreakActivityAttributes" Production
      firstInstall <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "streak-a" "install-a" install)
      secondInstall <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "streak-a" "install-a" install)
      firstInstall.installationId `shouldBe` secondInstall.installationId
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "streak-a" "install-a" startToken)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "streak-a" "install-a" startToken)
      denied <- runTestApp connection (Notifications.putPushToStartToken "streak-b" "install-a" startToken)
      denied `shouldSatisfy` isLeft
      now <- getCurrentTime
      let event1 = CardPlayedRequest "event-1" "install-a" (addUTCTime (-10) now)
      schedule1 <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "streak-a" event1)
      duplicate <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "streak-a" event1)
      duplicate.scheduleId `shouldBe` schedule1.scheduleId
      duplicate.generation `shouldBe` 1
      duplicate.delivery `shouldBe` ActivityKit
      [Only jobCount] <- query connection "SELECT count(*) FROM notification_jobs WHERE schedule_id=?::uuid" (Only schedule1.scheduleId) :: IO [Only Int]
      jobCount `shouldBe` 2
      let event2 = CardPlayedRequest "event-2" "install-a" now
      schedule2 <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "streak-a" event2)
      schedule2.generation `shouldBe` 2
      [Only cancelled] <- query connection "SELECT count(*) FROM notification_jobs WHERE schedule_id=?::uuid AND status='cancelled'" (Only schedule1.scheduleId) :: IO [Only Int]
      cancelled `shouldBe` 2
      outOfOrder <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "streak-a" (CardPlayedRequest "event-old" "install-a" (addUTCTime (-20) now)))
      outOfOrder.scheduleId `shouldBe` schedule2.scheduleId

    it "selects local fallback for iOS 17.1 and when Live Activities are disabled" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "fallback" "fallback@example.test" "password")
      let oldIOS = UpsertIOSInstallationRequest "ios" "17.1" "1" "1" True Sandbox
          token = PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "fallback" "fallback-install" oldIOS)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "fallback" "fallback-install" token)
      now <- getCurrentTime
      response <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "fallback" $ CardPlayedRequest "fallback-1" "fallback-install" now)
      response.delivery `shouldBe` LocalNotifications
      response.requiresLocalFallback `shouldBe` True
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs" () :: IO [Only Int]
      jobs `shouldBe` 0

    it "schedules short debug offsets for sandbox installations and the real offsets for production" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "timing" "timing@example.test" "password")
      let sandboxInstall = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Sandbox
          productionInstall = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Production
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "timing" "timing-sandbox" sandboxInstall)
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "timing" "timing-production" productionInstall)
      now <- getCurrentTime
      _ <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "timing" $ CardPlayedRequest "timing-event" "timing-sandbox" now)
      _ <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "timing" $ CardPlayedRequest "timing-event" "timing-production" now)
      [(sandboxStarts, sandboxCompletes)] <- query connection
        "SELECT starts_at,completes_at FROM streak_notification_schedules WHERE installation_id='timing-sandbox'" () :: IO [(UTCTime, UTCTime)]
      [(productionStarts, productionCompletes)] <- query connection
        "SELECT starts_at,completes_at FROM streak_notification_schedules WHERE installation_id='timing-production'" () :: IO [(UTCTime, UTCTime)]
      diffUTCTime sandboxStarts now `shouldSatisfy` closeTo 40
      diffUTCTime sandboxCompletes now `shouldSatisfy` closeTo 60
      diffUTCTime productionStarts now `shouldSatisfy` closeTo (47 * 60 * 60)
      diffUTCTime productionCompletes now `shouldSatisfy` closeTo (48 * 60 * 60)

    it "fans a card event out to all of the user's eligible installations" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "multi-device" "multi@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Production
          token = PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "multi-device" "multi-a" install)
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "multi-device" "multi-b" install)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "multi-device" "multi-a" token)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "multi-device" "multi-b" token)
      now <- getCurrentTime
      response <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "multi-device" $ CardPlayedRequest "multi-event" "multi-a" now)
      response.delivery `shouldBe` ActivityKit
      [Only schedules] <- query connection "SELECT count(*) FROM streak_notification_schedules WHERE username='multi-device' AND event_id='multi-event'" () :: IO [Only Int]
      schedules `shouldBe` 2
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs WHERE generation=1" () :: IO [Only Int]
      jobs `shouldBe` 4

    it "wakes an overdue completion when the activity update token arrives" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "late-token" "late@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Production
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "late-token" "late-install" install)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "late-token" "late-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "late-token" $ CardPlayedRequest "late-1" "late-install" now)
      _ <- execute connection "UPDATE streak_notification_schedules SET completes_at=now()-interval '1 minute',starts_at=now()-interval '2 minutes' WHERE schedule_id=?::uuid" (Only schedule.scheduleId)
      _ <- execute connection "UPDATE notification_jobs SET status='retry',next_attempt_at=now()+interval '1 hour' WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId)
      _ <- expectRight =<< runTestApp connection (Notifications.putActivityToken "late-token" schedule.scheduleId $ ActivityTokenRequest "late-install" "activity-1" 1 "bb00" Production)
      [(status, due)] <- query connection "SELECT status,COALESCE(next_attempt_at,run_at)<=now() FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId) :: IO [(Text, Bool)]
      status `shouldBe` "pending"
      due `shouldBe` True

    it "claims a due start once, routes it to the installation token, and advances state on APNs 200" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "worker-ok" "worker-ok@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Sandbox
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "worker-ok" "worker-install" install)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "worker-ok" "worker-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "worker-ok" $ CardPlayedRequest "worker-event" "worker-install" now)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId)
      requests <- newIORef []
      let transport request = modifyIORef' requests (request :) >> pure (APNSResponse 200 Nothing (Just "apns-attempt") Nothing)
      delivered <- runOnceWith connection transport "test-worker"
      delivered `shouldBe` 1
      [request] <- readIORef requests
      request.requestEnvironment `shouldBe` Sandbox
      request.requestToken `shouldBe` BS.pack [0, 170]
      [Only jobStatus] <- query connection "SELECT status FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId) :: IO [Only Text]
      jobStatus `shouldBe` "succeeded"
      [Only scheduleStatus] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      scheduleStatus `shouldBe` "starting"
      secondPass <- runOnceWith connection transport "other-worker"
      secondPass `shouldBe` 0

    it "retries transient APNs failures without advancing the schedule" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "worker-retry" "worker-retry@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Production
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "worker-retry" "retry-install" install)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "worker-retry" "retry-install" $ PushToStartTokenRequest "aa00" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "worker-retry" $ CardPlayedRequest "retry-event" "retry-install" now)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId)
      _ <- runOnceWith connection (\_ -> pure $ APNSResponse 503 (Just "ServiceUnavailable") Nothing Nothing) "test-worker"
      [(jobStatus, hasNextAttempt)] <- query connection "SELECT status,next_attempt_at IS NOT NULL FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId) :: IO [(Text, Bool)]
      jobStatus `shouldBe` "retry"
      hasNextAttempt `shouldBe` True
      [Only scheduleStatus] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      scheduleStatus `shouldBe` "scheduled"
