{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.StreakNotificationSpec (spec) where

import Data.Aeson (Value(..), eitherDecode, encode)
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
  describe "installation request decoding" $ do
    it "accepts old clients that omit the local scheduling capability" $ do
      let decoded = eitherDecode "{\"platform\":\"ios\",\"osVersion\":\"18.6\",\"appVersion\":\"1.4.0\",\"buildNumber\":\"82\",\"liveActivitiesEnabled\":true,\"apnsEnvironment\":\"production\"}" :: Either String UpsertIOSInstallationRequest
      request <- expectRight decoded
      request.supportsLocallyScheduledLiveActivities `shouldBe` Nothing

  describe "card-play request decoding" $ do
    it "defaults omitted timing profiles and rejects unknown profiles" $ do
      let legacy = "{\"eventId\":\"play\",\"installationId\":\"device\",\"playedAt\":\"2026-09-18T12:00:00Z\"}"
          invalid = "{\"eventId\":\"play\",\"installationId\":\"device\",\"playedAt\":\"2026-09-18T12:00:00Z\",\"timingProfile\":\"sandbox\"}"
      request <- expectRight (eitherDecode legacy :: Either String CardPlayedRequest)
      request.timingProfile `shouldBe` Nothing
      (eitherDecode invalid :: Either String CardPlayedRequest) `shouldSatisfy` isLeft

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
      json `shouldContain` "\"body\":\"Your streak expires in ...\""
    it "uses update and end events with a final complete state" $ do
      LBS8.unpack (encode $ completePayload now completes) `shouldContain` "\"event\":\"update\""
      LBS8.unpack (encode $ completePayload now completes) `shouldContain` "\"isComplete\":true"
      let final = LBS8.unpack $ encode $ endPayload now completes True
      final `shouldContain` "\"event\":\"end\""
      final `shouldContain` "\"isComplete\":true"
      final `shouldContain` "\"dismissal-date\""

  describe "notification repository" $ do
    it "is idempotent, enforces ownership, and replaces schedules atomically" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "streak-a" "a@example.test" "password")
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "streak-b" "b@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.6" "1.4.0" "82" True Nothing Production
          startToken = PushToStartTokenRequest "A0ff" "StreakActivityAttributes" Production
      firstInstall <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "streak-a" "install-a" install)
      secondInstall <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "streak-a" "install-a" install)
      firstInstall.installationId `shouldBe` secondInstall.installationId
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "streak-a" "install-a" startToken)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "streak-a" "install-a" startToken)
      denied <- runTestAppWithAPNS connection (Notifications.putPushToStartToken "streak-b" "install-a" startToken)
      denied `shouldSatisfy` isLeft
      now <- getCurrentTime
      let event1 = CardPlayedRequest "event-1" "install-a" (addUTCTime (-10) now) Nothing
      schedule1 <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "streak-a" event1)
      duplicate <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "streak-a" event1)
      duplicate.scheduleId `shouldBe` schedule1.scheduleId
      duplicate.generation `shouldBe` 1
      duplicate.delivery `shouldBe` ActivityKit
      [Only jobCount] <- query connection "SELECT count(*) FROM notification_jobs WHERE schedule_id=?::uuid" (Only schedule1.scheduleId) :: IO [Only Int]
      jobCount `shouldBe` 2
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only schedule1.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete", Only "start"]
      let event2 = CardPlayedRequest "event-2" "install-a" now Nothing
      schedule2 <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "streak-a" event2)
      schedule2.generation `shouldBe` 2
      [Only cancelled] <- query connection "SELECT count(*) FROM notification_jobs WHERE schedule_id=?::uuid AND status='cancelled'" (Only schedule1.scheduleId) :: IO [Only Int]
      cancelled `shouldBe` 2
      outOfOrder <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "streak-a" (CardPlayedRequest "event-old" "install-a" (addUTCTime (-20) now) Nothing))
      outOfOrder.scheduleId `shouldBe` schedule2.scheduleId

    it "updates local scheduling support in place and omits only the start job when enabled" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "local-schedule" "local@example.test" "password")
      let unsupported = UpsertIOSInstallationRequest "ios" "26.0" "2" "100" True (Just False) Production
          supported = UpsertIOSInstallationRequest "ios" "26.0" "2" "101" True (Just True) Production
      first <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "local-schedule" "stable-install" unsupported)
      second <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "local-schedule" "stable-install" supported)
      second.installationId `shouldBe` first.installationId
      [Only capability] <- query connection "SELECT supports_locally_scheduled_live_activities FROM ios_notification_installations WHERE installation_id='stable-install'" () :: IO [Only Bool]
      capability `shouldBe` True
      now <- getCurrentTime
      response <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "local-schedule" $ CardPlayedRequest "local-event" "stable-install" now Nothing)
      response.delivery `shouldBe` ActivityKit
      response.requiresLocalFallback `shouldBe` False
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only response.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete"]

    it "keeps the legacy start, complete, and end job flow when capability is missing" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "legacy-flow" "legacy@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "legacy-flow" "legacy-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "legacy-flow" "legacy-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "legacy-flow" $ CardPlayedRequest "legacy-event" "legacy-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "legacy-flow" schedule.scheduleId $ ActivityTokenRequest "legacy-install" "legacy-activity" 1 "bb00" Production)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId)
      delivered <- runOnceWith connection (\_ -> pure $ APNSResponse 200 Nothing (Just "legacy-complete") Nothing) "legacy-worker"
      delivered `shouldBe` 1
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only schedule.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete", Only "end", Only "start"]

    it "selects local fallback for iOS 17.1 and when Live Activities are disabled" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "fallback" "fallback@example.test" "password")
      let oldIOS = UpsertIOSInstallationRequest "ios" "17.1" "1" "1" True Nothing Sandbox
          token = PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "fallback" "fallback-install" oldIOS)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "fallback" "fallback-install" token)
      now <- getCurrentTime
      response <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "fallback" $ CardPlayedRequest "fallback-1" "fallback-install" now Nothing)
      response.delivery `shouldBe` LocalNotifications
      response.requiresLocalFallback `shouldBe` True
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs" () :: IO [Only Int]
      jobs `shouldBe` 0

    it "uses production timing by default for both APNs environments" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "timing" "timing@example.test" "password")
      let sandboxInstall = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Sandbox
          productionInstall = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "timing" "timing-sandbox" sandboxInstall)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "timing" "timing-production" productionInstall)
      now <- getCurrentTime
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "timing" $ CardPlayedRequest "timing-event" "timing-sandbox" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "timing" $ CardPlayedRequest "timing-event" "timing-production" now Nothing)
      [(sandboxStarts, sandboxCompletes)] <- query connection
        "SELECT starts_at,completes_at FROM streak_notification_schedules WHERE installation_id='timing-sandbox'" () :: IO [(UTCTime, UTCTime)]
      [(productionStarts, productionCompletes)] <- query connection
        "SELECT starts_at,completes_at FROM streak_notification_schedules WHERE installation_id='timing-production'" () :: IO [(UTCTime, UTCTime)]
      diffUTCTime sandboxStarts now `shouldSatisfy` closeTo (47 * 60 * 60)
      diffUTCTime sandboxCompletes now `shouldSatisfy` closeTo (48 * 60 * 60)
      diffUTCTime productionStarts now `shouldSatisfy` closeTo (47 * 60 * 60)
      diffUTCTime productionCompletes now `shouldSatisfy` closeTo (48 * 60 * 60)

    it "uses explicit preview timing independently of production APNs routing" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "preview" "preview@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just False) Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "preview" "preview-install" install)
      now <- getCurrentTime
      response <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "preview" $ CardPlayedRequest "preview-event" "preview-install" now (Just AcceleratedPreviewTiming))
      diffUTCTime response.startsAt now `shouldSatisfy` closeTo 15
      diffUTCTime response.completesAt now `shouldSatisfy` closeTo 25

    it "keeps fallback delivery when the APNs worker is not configured" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestApp connection (Users.insert $ mkTestUser "no-apns" "no-apns@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just False) Production
      _ <- expectRight =<< runTestApp connection (Notifications.upsertInstallation "no-apns" "no-apns-install" install)
      _ <- expectRight =<< runTestApp connection (Notifications.putPushToStartToken "no-apns" "no-apns-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      response <- expectRight =<< runTestApp connection (Notifications.recordCardPlayed "no-apns" $ CardPlayedRequest "no-apns-event" "no-apns-install" now Nothing)
      response.delivery `shouldBe` LocalNotifications
      response.requiresLocalFallback `shouldBe` True
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs" () :: IO [Only Int]
      jobs `shouldBe` 0

    it "repairs a tokenless iOS 26 fallback on replay without duplicating successful starts" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "repair" "repair@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just False) Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "repair" "repair-install" install)
      now <- getCurrentTime
      let event = CardPlayedRequest "repair-event" "repair-install" now Nothing
      original <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "repair" event)
      original.delivery `shouldBe` LocalNotifications
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "repair" "repair-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      repaired <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "repair" event)
      repaired.scheduleId `shouldBe` original.scheduleId
      repaired.delivery `shouldBe` ActivityKit
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only repaired.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete", Only "start"]
      _ <- execute connection "UPDATE notification_jobs SET status='succeeded' WHERE schedule_id=?::uuid AND job_type='start'" (Only repaired.scheduleId)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "repair" event)
      [Only status] <- query connection "SELECT status FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='start'" (Only repaired.scheduleId) :: IO [Only Text]
      status `shouldBe` "succeeded"
      _ <- execute connection "UPDATE streak_notification_schedules SET status='superseded' WHERE schedule_id=?::uuid" (Only repaired.scheduleId)
      _ <- execute connection "DELETE FROM notification_jobs WHERE schedule_id=?::uuid" (Only repaired.scheduleId)
      obsolete <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "repair" event)
      obsolete.delivery `shouldBe` LocalNotifications
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs" () :: IO [Only Int]
      jobs `shouldBe` 0

    it "adds the missing start job when an old locally scheduled installation upgrades" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "upgrade" "upgrade@example.test" "password")
      let old = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Production
          updated = old { supportsLocallyScheduledLiveActivities = Just False }
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "upgrade" "upgrade-install" old)
      now <- getCurrentTime
      let event = CardPlayedRequest "upgrade-event" "upgrade-install" now Nothing
      original <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "upgrade" event)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "upgrade" "upgrade-install" updated)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "upgrade" "upgrade-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      upgraded <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "upgrade" event)
      upgraded.scheduleId `shouldBe` original.scheduleId
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only upgraded.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete", Only "start"]

    it "does not duplicate a token-backed local activity when its installation upgrades" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "active-upgrade" "active-upgrade@example.test" "password")
      let old = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Production
          updated = old { supportsLocallyScheduledLiveActivities = Just False }
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "active-upgrade" "active-upgrade-install" old)
      now <- getCurrentTime
      let event = CardPlayedRequest "active-upgrade-event" "active-upgrade-install" now Nothing
      original <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "active-upgrade" event)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "active-upgrade" original.scheduleId $ ActivityTokenRequest "active-upgrade-install" "existing-activity" 1 "bb00" Production)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "active-upgrade" "active-upgrade-install" updated)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "active-upgrade" "active-upgrade-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "active-upgrade" event)
      jobTypes <- query connection "SELECT job_type FROM notification_jobs WHERE schedule_id=?::uuid ORDER BY job_type" (Only original.scheduleId) :: IO [Only Text]
      jobTypes `shouldBe` [Only "complete"]

    it "fans a card event out to all of the user's eligible installations" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "multi-device" "multi@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
          token = PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "multi-device" "multi-a" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "multi-device" "multi-b" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "multi-device" "multi-a" token)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "multi-device" "multi-b" token)
      now <- getCurrentTime
      response <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "multi-device" $ CardPlayedRequest "multi-event" "multi-a" now Nothing)
      response.delivery `shouldBe` ActivityKit
      [Only schedules] <- query connection "SELECT count(*) FROM streak_notification_schedules WHERE username='multi-device' AND event_id='multi-event'" () :: IO [Only Int]
      schedules `shouldBe` 2
      [Only jobs] <- query connection "SELECT count(*) FROM notification_jobs WHERE generation=1" () :: IO [Only Int]
      jobs `shouldBe` 4

    it "wakes an overdue completion when the activity update token arrives" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "late-token" "late@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "late-token" "late-install" install)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "late-token" $ CardPlayedRequest "late-1" "late-install" now Nothing)
      _ <- execute connection "UPDATE streak_notification_schedules SET completes_at=now()-interval '1 minute',starts_at=now()-interval '2 minutes' WHERE schedule_id=?::uuid" (Only schedule.scheduleId)
      _ <- execute connection "UPDATE notification_jobs SET status='retry',next_attempt_at=now()+interval '1 hour' WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "late-token" schedule.scheduleId $ ActivityTokenRequest "late-install" "activity-1" 1 "bb00" Production)
      [Only activityCount] <- query connection "SELECT count(*) FROM streak_live_activities WHERE schedule_id=?::uuid AND token_valid" (Only schedule.scheduleId) :: IO [Only Int]
      activityCount `shouldBe` 1
      [(status, due)] <- query connection "SELECT status,COALESCE(next_attempt_at,run_at)<=now() FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId) :: IO [(Text, Bool)]
      status `shouldBe` "pending"
      due `shouldBe` True

    it "claims a due start once, routes it to the installation token, and advances state on APNs 200" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "worker-ok" "worker-ok@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "worker-ok" "worker-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "worker-ok" "worker-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "worker-ok" $ CardPlayedRequest "worker-event" "worker-install" now Nothing)
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
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "worker-retry" "worker-retry@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "worker-retry" "retry-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "worker-retry" "retry-install" $ PushToStartTokenRequest "aa00" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "worker-retry" $ CardPlayedRequest "retry-event" "retry-install" now Nothing)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId)
      _ <- runOnceWith connection (\_ -> pure $ APNSResponse 503 (Just "ServiceUnavailable") Nothing Nothing) "test-worker"
      [(jobStatus, hasNextAttempt)] <- query connection "SELECT status,next_attempt_at IS NOT NULL FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId) :: IO [(Text, Bool)]
      jobStatus `shouldBe` "retry"
      hasNextAttempt `shouldBe` True
      [Only scheduleStatus] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      scheduleStatus `shouldBe` "scheduled"

    it "retires an active schedule when an installation changes owner" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "old-owner" "old-owner@example.test" "password")
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "new-owner" "new-owner@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "old-owner" "transferred-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "old-owner" "transferred-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "old-owner" $ CardPlayedRequest "transfer-event" "transferred-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "old-owner" schedule.scheduleId $ ActivityTokenRequest "transferred-install" "transfer-activity" 1 "bb00" Production)

      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "new-owner" "transferred-install" install)

      [Only owner] <- query connection "SELECT username FROM ios_notification_installations WHERE installation_id='transferred-install'" () :: IO [Only String]
      owner `shouldBe` "new-owner"
      [(scheduleStatus, endJobs)] <- query connection "SELECT s.status,count(j.job_id) FROM streak_notification_schedules s LEFT JOIN notification_jobs j ON j.schedule_id=s.schedule_id AND j.job_type='end' WHERE s.schedule_id=?::uuid GROUP BY s.status" (Only schedule.scheduleId) :: IO [(Text, Int)]
      (scheduleStatus, endJobs) `shouldBe` ("superseded", 1)

    it "cancels work and queues an activity end when Live Activities are disabled" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "disable-live" "disable-live@example.test" "password")
      let enabled = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
          disabled = enabled { liveActivitiesEnabled = False }
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "disable-live" "disable-install" enabled)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "disable-live" "disable-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "disable-live" $ CardPlayedRequest "disable-event" "disable-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "disable-live" schedule.scheduleId $ ActivityTokenRequest "disable-install" "disable-activity" 1 "bb00" Production)

      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "disable-live" "disable-install" disabled)

      [Only status] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      status `shouldBe` "superseded"
      [Only endJobs] <- query connection "SELECT count(*) FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='end'" (Only schedule.scheduleId) :: IO [Only Int]
      endJobs `shouldBe` 1

    it "immediately ends an activity token submitted for a superseded schedule" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "late-activity" "late-activity@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "late-activity" "late-activity-install" install)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "late-activity" $ CardPlayedRequest "late-activity-event" "late-activity-install" now Nothing)
      _ <- execute connection "UPDATE streak_notification_schedules SET status='superseded' WHERE schedule_id=?::uuid" (Only schedule.scheduleId)

      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "late-activity" schedule.scheduleId $ ActivityTokenRequest "late-activity-install" "late-arriving-activity" 1 "bb00" Production)

      [(jobType, immediate)] <- query connection "SELECT job_type,immediate_dismissal FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='end'" (Only schedule.scheduleId) :: IO [(Text, Bool)]
      (jobType, immediate) `shouldBe` ("end", True)

    it "deletes an owned installation and invalidates its active activity" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "delete-install" "delete-install@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Production
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "delete-install" "deleted-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "delete-install" "deleted-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Production)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "delete-install" $ CardPlayedRequest "delete-install-event" "deleted-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "delete-install" schedule.scheduleId $ ActivityTokenRequest "deleted-install" "deleted-activity" 1 "bb00" Production)

      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.deleteInstallation "delete-install" "deleted-install")

      [(deleted, enabled)] <- query connection "SELECT deleted_at IS NOT NULL,live_activities_enabled FROM ios_notification_installations WHERE installation_id='deleted-install'" () :: IO [(Bool, Bool)]
      (deleted, enabled) `shouldBe` (True, False)
      [Only valid] <- query connection "SELECT token_valid FROM streak_live_activities WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Bool]
      valid `shouldBe` False

    it "cancels a claimed job whose schedule has become stale" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "stale-job" "stale-job@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "stale-job" "stale-job-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "stale-job" "stale-job-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "stale-job" $ CardPlayedRequest "stale-job-event" "stale-job-install" now Nothing)
      _ <- execute connection "UPDATE streak_notification_schedules SET status='superseded' WHERE schedule_id=?::uuid" (Only schedule.scheduleId)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId)

      processed <- runOnceWith connection (\_ -> expectationFailure "stale jobs must not reach APNs" >> error "unreachable") "stale-worker"
      processed `shouldBe` 1
      [Only status] <- query connection "SELECT status FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId) :: IO [Only Text]
      status `shouldBe` "cancelled"

    it "finishes a tokenless end job without contacting APNs" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "tokenless-end" "tokenless-end@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "tokenless-end" "tokenless-end-install" install)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "tokenless-end" $ CardPlayedRequest "tokenless-end-event" "tokenless-end-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "tokenless-end" schedule.scheduleId $ ActivityTokenRequest "tokenless-end-install" "tokenless-end-activity" 1 "bb00" Sandbox)
      _ <- execute connection "UPDATE streak_live_activities SET token_valid=false WHERE schedule_id=?::uuid" (Only schedule.scheduleId)
      _ <- execute connection "UPDATE notification_jobs SET job_type='end',run_at=now() WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId)

      processed <- runOnceWith connection (\_ -> expectationFailure "a tokenless end must not reach APNs" >> error "unreachable") "end-worker"
      processed `shouldBe` 1
      [Only jobStatus] <- query connection "SELECT status FROM notification_jobs WHERE schedule_id=?::uuid AND job_type='end'" (Only schedule.scheduleId) :: IO [Only Text]
      jobStatus `shouldBe` "succeeded"
      [Only scheduleStatus] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      scheduleStatus `shouldBe` "ended"

    it "invalidates a rejected push-to-start token and fails the schedule" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "bad-start" "bad-start@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "18.0" "1" "1" True Nothing Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "bad-start" "bad-start-install" install)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putPushToStartToken "bad-start" "bad-start-install" $ PushToStartTokenRequest "00aa" "StreakActivityAttributes" Sandbox)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "bad-start" $ CardPlayedRequest "bad-start-event" "bad-start-install" now Nothing)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='start'" (Only schedule.scheduleId)

      _ <- runOnceWith connection (\_ -> pure $ APNSResponse 410 (Just "Unregistered") Nothing Nothing) "bad-start-worker"

      [Only tokenCleared] <- query connection "SELECT push_to_start_token IS NULL FROM ios_notification_installations WHERE installation_id='bad-start-install'" () :: IO [Only Bool]
      tokenCleared `shouldBe` True
      [Only scheduleStatus] <- query connection "SELECT status FROM streak_notification_schedules WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Text]
      scheduleStatus `shouldBe` "failed"

    it "invalidates a rejected activity update token" $ withCleanDb $ \connection -> do
      _ <- expectRight =<< runTestAppWithAPNS connection (Users.insert $ mkTestUser "bad-update" "bad-update@example.test" "password")
      let install = UpsertIOSInstallationRequest "ios" "26.0" "1" "1" True (Just True) Sandbox
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.upsertInstallation "bad-update" "bad-update-install" install)
      now <- getCurrentTime
      schedule <- expectRight =<< runTestAppWithAPNS connection (Notifications.recordCardPlayed "bad-update" $ CardPlayedRequest "bad-update-event" "bad-update-install" now Nothing)
      _ <- expectRight =<< runTestAppWithAPNS connection (Notifications.putActivityToken "bad-update" schedule.scheduleId $ ActivityTokenRequest "bad-update-install" "bad-update-activity" 1 "bb00" Sandbox)
      _ <- execute connection "UPDATE notification_jobs SET run_at=now() WHERE schedule_id=?::uuid AND job_type='complete'" (Only schedule.scheduleId)

      _ <- runOnceWith connection (\_ -> pure $ APNSResponse 400 (Just "BadDeviceToken") Nothing Nothing) "bad-update-worker"

      [Only valid] <- query connection "SELECT token_valid FROM streak_live_activities WHERE schedule_id=?::uuid" (Only schedule.scheduleId) :: IO [Only Bool]
      valid `shouldBe` False
