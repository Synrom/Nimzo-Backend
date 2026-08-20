{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.StreakNotification
  ( decodeActivityToken
  , upsertInstallation
  , putPushToStartToken
  , recordCardPlayed
  , putActivityToken
  , deleteInstallation
  ) where

import Control.Monad (forM, unless, when)
import qualified Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, isDigit)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Types (Binary(..))
import Models.StreakNotification
import Repo.Classes
import App.Error (AppError(..))

data InstallationRow = InstallationRow Text Text Bool Bool APNSEnvironment (Maybe ByteString)
instance FromRow InstallationRow where
  fromRow = InstallationRow <$> field <*> field <*> field <*> field <*> (parseEnvironment <$> field) <*> (fmap fromBinary <$> field)
    where fromBinary (Binary bytes) = bytes

data ScheduleRow = ScheduleRow Text Integer UTCTime UTCTime UTCTime Text
instance FromRow ScheduleRow where
  fromRow = ScheduleRow <$> field <*> field <*> field <*> field <*> field <*> field

parseEnvironment :: Text -> APNSEnvironment
parseEnvironment "sandbox" = Sandbox
parseEnvironment _ = Production

bad :: String -> Either AppError a
bad = Left . BadRequest

decodeActivityToken :: Text -> Either AppError ByteString
decodeActivityToken value
  | T.null value = bad "ActivityKit token must not be empty."
  | T.length value > 1024 = bad "ActivityKit token is too large."
  | odd (T.length value) = bad "ActivityKit token must contain an even number of hexadecimal characters."
  | not (T.all isHexDigit value) = bad "ActivityKit token must be hexadecimal."
  | otherwise = Right . BS.pack $ pairs (T.unpack value)
  where
    pairs [] = []
    pairs (a:b:rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : pairs rest
    pairs _ = []

throwEither :: MonadApp m => Either AppError a -> m a
throwEither = either (Control.Monad.Except.throwError) pure

ensure :: MonadApp m => Bool -> AppError -> m ()
ensure condition err = unless condition (Control.Monad.Except.throwError err)

validShort :: Text -> Bool
validShort value = T.length value <= 64

upsertInstallation :: MonadDB m => String -> Text -> UpsertIOSInstallationRequest -> m IOSInstallationResponse
upsertInstallation username installationId request = withTransaction $ do
  ensure (not (T.null installationId) && T.length installationId <= 128) (BadRequest "Invalid installationId.")
  ensure (request.platform == "ios") (BadRequest "platform must be ios.")
  ensure (all validShort [request.osVersion, request.appVersion, request.buildNumber]) (BadRequest "Version fields may contain at most 64 characters.")
  previousOwners <- runQuery
    "SELECT username FROM ios_notification_installations WHERE installation_id = ? FOR UPDATE"
    (Only installationId) :: MonadDB m => m [Only String]
  case previousOwners of
    Only oldOwner : _ | oldOwner /= username -> do
      _ <- execute "UPDATE notification_jobs SET status = 'cancelled', updated_at = now() WHERE installation_id = ? AND status IN ('pending','retry','running')" (Only installationId)
      _ <- execute "UPDATE streak_notification_schedules SET status = 'superseded', updated_at = now() WHERE installation_id = ? AND status IN ('scheduled','starting','active','complete','ending')" (Only installationId)
      _ <- execute "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at,target_token,target_environment,immediate_dismissal) SELECT s.schedule_id,s.installation_id,s.generation,'end',now(),a.update_token,a.apns_environment,true FROM streak_notification_schedules s JOIN streak_live_activities a ON a.schedule_id=s.schedule_id WHERE s.installation_id=? AND a.token_valid ON CONFLICT (schedule_id,job_type) DO UPDATE SET status='pending',run_at=now(),target_token=EXCLUDED.target_token,target_environment=EXCLUDED.target_environment,immediate_dismissal=true,updated_at=now()" (Only installationId)
      pure ()
    _ -> pure ()
  _ <- execute
    "INSERT INTO ios_notification_installations (installation_id,username,os_version,app_version,build_number,live_activities_enabled,supports_locally_scheduled_live_activities,apns_environment,last_seen_at) VALUES (?,?,?,?,?,?,?,?,now()) ON CONFLICT (installation_id) DO UPDATE SET username=EXCLUDED.username,os_version=EXCLUDED.os_version,app_version=EXCLUDED.app_version,build_number=EXCLUDED.build_number,live_activities_enabled=EXCLUDED.live_activities_enabled,supports_locally_scheduled_live_activities=EXCLUDED.supports_locally_scheduled_live_activities,apns_environment=EXCLUDED.apns_environment,last_seen_at=now(),updated_at=now(),deleted_at=NULL,push_to_start_token=CASE WHEN ios_notification_installations.username=EXCLUDED.username AND ios_notification_installations.apns_environment=EXCLUDED.apns_environment THEN ios_notification_installations.push_to_start_token ELSE NULL END,token_updated_at=CASE WHEN ios_notification_installations.username=EXCLUDED.username AND ios_notification_installations.apns_environment=EXCLUDED.apns_environment THEN ios_notification_installations.token_updated_at ELSE NULL END"
    (installationId, username, request.osVersion, request.appVersion, request.buildNumber, request.liveActivitiesEnabled, fromMaybe False request.supportsLocallyScheduledLiveActivities, environmentText request.apnsEnvironment)
  unless request.liveActivitiesEnabled $ do
    _ <- execute "UPDATE notification_jobs SET status='cancelled',updated_at=now() WHERE installation_id=? AND status IN ('pending','retry','running')" (Only installationId)
    _ <- execute "UPDATE streak_notification_schedules SET status='superseded',updated_at=now() WHERE installation_id=? AND status IN ('scheduled','starting','active','complete','ending')" (Only installationId)
    _ <- execute "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at,target_token,target_environment,immediate_dismissal) SELECT s.schedule_id,s.installation_id,s.generation,'end',now(),a.update_token,a.apns_environment,true FROM streak_notification_schedules s JOIN streak_live_activities a ON a.schedule_id=s.schedule_id WHERE s.installation_id=? AND a.token_valid ON CONFLICT (schedule_id,job_type) DO UPDATE SET status='pending',run_at=now(),target_token=EXCLUDED.target_token,target_environment=EXCLUDED.target_environment,immediate_dismissal=true,updated_at=now()" (Only installationId)
    pure ()
  now <- liftIO getCurrentTime
  pure $ IOSInstallationResponse installationId now

putPushToStartToken :: MonadDB m => String -> Text -> PushToStartTokenRequest -> m ()
putPushToStartToken username installationId request = withTransaction $ do
  token <- throwEither (decodeActivityToken request.token)
  ensure (request.attributesType == "StreakActivityAttributes") (BadRequest "Unknown attributesType.")
  rows <- runQuery "SELECT apns_environment FROM ios_notification_installations WHERE installation_id=? AND username=? AND deleted_at IS NULL FOR UPDATE" (installationId, username) :: MonadDB m => m [Only Text]
  environment <- case rows of
    Only value : _ -> pure value
    _ -> Control.Monad.Except.throwError (NotFound "Installation not found.")
  ensure (environment == environmentText request.apnsEnvironment) (BadRequest "APNs environment does not match the installation.")
  _ <- execute "UPDATE ios_notification_installations SET push_to_start_token=?,token_updated_at=now(),updated_at=now() WHERE installation_id=? AND username=?" (Binary token, installationId, username)
  pure ()

scheduleResponse :: InstallationRow -> ScheduleRow -> StreakScheduleResponse
scheduleResponse installation (ScheduleRow sid gen played starts completes _) =
  let eligible = installationEligible installation
  in StreakScheduleResponse sid gen played starts completes
       (if eligible then ActivityKit else LocalNotifications) (not eligible)

installationEligible :: InstallationRow -> Bool
installationEligible (InstallationRow _ os enabled locallyScheduled _ token) =
  enabled && (locallyScheduled || isJust token) && iosAtLeast172 os

supportsLocalScheduling :: InstallationRow -> Bool
supportsLocalScheduling (InstallationRow _ _ _ supported _ _) = supported

iosAtLeast172 :: Text -> Bool
iosAtLeast172 raw = case map readNumber (take 2 (T.splitOn "." raw)) of
  major:minor:_ -> major > 17 || (major == 17 && minor >= 2)
  major:_ -> major > 17
  _ -> False
  where readNumber part = case T.span isDigit part of
          (digits, _) | not (T.null digits) -> read (T.unpack digits) :: Int
          _ -> 0

findInstallation :: MonadDB m => String -> Text -> Bool -> m InstallationRow
findInstallation username installationId lock = do
  let suffix = if lock then " FOR UPDATE" else ""
  rows <- runQuery
    ("SELECT installation_id,os_version,live_activities_enabled,supports_locally_scheduled_live_activities,apns_environment,push_to_start_token FROM ios_notification_installations WHERE installation_id=? AND username=? AND deleted_at IS NULL" <> suffix)
    (installationId, username)
  case rows of
    row : _ -> pure row
    _ -> Control.Monad.Except.throwError (NotFound "Installation not found.")

scheduleTimes :: APNSEnvironment -> UTCTime -> (UTCTime, UTCTime)
-- Mirrors the accelerated `__DEV__` offsets in the app's
-- `src/notifications/streak/designs.ts`. The completion has to stay inside
-- iOS's ~30s background execution allowance: a locally scheduled fallback
-- activity can only swap the countdown for its completion presentation by
-- running `Activity.update` itself, and a later endpoint is never reached.
scheduleTimes Sandbox playedAt = (addUTCTime 15 playedAt, addUTCTime 25 playedAt)
scheduleTimes Production playedAt = (addUTCTime (47 * 60 * 60) playedAt, addUTCTime (48 * 60 * 60) playedAt)

recordCardPlayed :: MonadDB m => String -> CardPlayedRequest -> m StreakScheduleResponse
recordCardPlayed username request = withTransaction $ do
  ensure (not (T.null request.eventId) && T.length request.eventId <= 128) (BadRequest "Invalid eventId.")
  now <- liftIO getCurrentTime
  ensure (request.playedAt <= addUTCTime 300 now) (BadRequest "playedAt is too far in the future.")
  _ <- runQuery "SELECT username FROM users WHERE username=? FOR UPDATE" (Only username) :: MonadDB m => m [Only String]
  installation <- findInstallation username request.installationId True
  duplicates <- runQuery
    "SELECT schedule_id::text,generation,last_played_at,starts_at,completes_at,status FROM streak_notification_schedules WHERE username=? AND event_id=? AND installation_id=?"
    (username, request.eventId, request.installationId)
  case duplicates of
    existing : _ -> pure (scheduleResponse installation existing)
    [] -> do
      current <- runQuery
        "SELECT schedule_id::text,generation,last_played_at,starts_at,completes_at,status FROM streak_notification_schedules WHERE username=? AND installation_id=? ORDER BY generation DESC LIMIT 1 FOR UPDATE"
        (username, request.installationId)
      case current of
        existing@(ScheduleRow _ _ lastPlayed _ _ _) : _ | request.playedAt <= lastPlayed ->
          pure (scheduleResponse installation existing)
        _ -> createSchedule installation current now
  where
    createSchedule installation _ _ = do
      generations <- runQuery "SELECT COALESCE(MAX(generation),0) FROM streak_notification_schedules WHERE username=?" (Only username) :: MonadDB m => m [Only Integer]
      let oldGeneration = case generations of Only value : _ -> value; _ -> 0
          generation = oldGeneration + 1
      _ <- execute "UPDATE notification_jobs SET status='cancelled',updated_at=now() WHERE schedule_id IN (SELECT schedule_id FROM streak_notification_schedules WHERE username=?) AND status IN ('pending','retry','running')" (Only username)
      _ <- execute "UPDATE streak_notification_schedules SET status='superseded',updated_at=now() WHERE username=? AND status IN ('scheduled','starting','active','complete','ending')" (Only username)
      _ <- execute
        "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at,status,target_token,target_environment,immediate_dismissal) SELECT s.schedule_id,s.installation_id,s.generation,'end',now(),'pending',a.update_token,a.apns_environment,true FROM streak_notification_schedules s JOIN streak_live_activities a ON a.schedule_id=s.schedule_id WHERE s.username=? AND s.status='superseded' AND a.token_valid ON CONFLICT (schedule_id,job_type) DO UPDATE SET status='pending',run_at=now(),target_token=EXCLUDED.target_token,target_environment=EXCLUDED.target_environment,immediate_dismissal=true,updated_at=now()"
        (Only username)
      installations <- runQuery "SELECT installation_id,os_version,live_activities_enabled,supports_locally_scheduled_live_activities,apns_environment,push_to_start_token FROM ios_notification_installations WHERE username=? AND deleted_at IS NULL" (Only username)
      created <- forM installations $ \target@(InstallationRow targetId _ _ _ targetEnvironment _) -> do
        let (starts, completes) = scheduleTimes targetEnvironment request.playedAt
        inserted <- runQuery
          "INSERT INTO streak_notification_schedules (username,installation_id,event_id,generation,last_played_at,starts_at,completes_at,status) VALUES (?,?,?,?,?,?,?,'scheduled') RETURNING schedule_id::text,generation,last_played_at,starts_at,completes_at,status"
          (username, targetId, request.eventId, generation, request.playedAt, starts, completes)
        schedule <- case inserted of row : _ -> pure row; _ -> Control.Monad.Except.throwError (Internal "Failed to create streak schedule.")
        when (installationEligible target) $ do
          let ScheduleRow sid _ _ _ _ _ = schedule
          unless (supportsLocalScheduling target) $ do
            _ <- execute
              "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at) VALUES (?::uuid,?,?,'start',?)"
              (sid, targetId, generation, starts)
            pure ()
          _ <- execute
            "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at) VALUES (?::uuid,?,?,'complete',?)"
            (sid, targetId, generation, completes)
          pure ()
        pure (targetId, schedule)
      schedule <- case [row | (targetId, row) <- created, targetId == request.installationId] of
        row : _ -> pure row
        _ -> Control.Monad.Except.throwError (Internal "Failed to create calling installation schedule.")
      pure (scheduleResponse installation schedule)

putActivityToken :: MonadDB m => String -> Text -> ActivityTokenRequest -> m ()
putActivityToken username scheduleId request = withTransaction $ do
  token <- throwEither (decodeActivityToken request.token)
  ensure (validUUIDText scheduleId) (BadRequest "Invalid scheduleId.")
  ensure (not (T.null request.activityId) && T.length request.activityId <= 128) (BadRequest "Invalid activityId.")
  rows <- runQuery
    "SELECT s.status,i.apns_environment,s.completes_at FROM streak_notification_schedules s JOIN ios_notification_installations i ON i.installation_id=s.installation_id WHERE s.schedule_id=?::uuid AND s.username=? AND s.installation_id=? AND s.generation=? FOR UPDATE OF s"
    (scheduleId, username, request.installationId, request.generation) :: MonadDB m => m [(Text, Text, UTCTime)]
  (status, environment, completesAt) <- case rows of
    row : _ -> pure row
    _ -> Control.Monad.Except.throwError (NotFound "Schedule not found.")
  ensure (environment == environmentText request.apnsEnvironment) (BadRequest "APNs environment does not match the installation.")
  now <- liftIO getCurrentTime
  if status == "superseded" || status == "ended"
    then do
      _ <- execute
        "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at,target_token,target_environment,immediate_dismissal) VALUES (?::uuid,?,?,'end',now(),?,?,true) ON CONFLICT (schedule_id,job_type) DO UPDATE SET status='pending',run_at=now(),target_token=EXCLUDED.target_token,target_environment=EXCLUDED.target_environment,immediate_dismissal=true,updated_at=now()"
        (scheduleId, request.installationId, request.generation, Binary token, environment)
      pure ()
    else do
      _ <- execute
        "INSERT INTO streak_live_activities (activity_id,schedule_id,installation_id,generation,update_token,apns_environment) VALUES (?,?::uuid,?,?,?,?) ON CONFLICT (schedule_id) DO UPDATE SET activity_id=EXCLUDED.activity_id,update_token=EXCLUDED.update_token,apns_environment=EXCLUDED.apns_environment,token_valid=true,token_updated_at=now(),updated_at=now()"
        (request.activityId, scheduleId, request.installationId, request.generation, Binary token, environment)
      _ <- execute "UPDATE streak_notification_schedules SET status='active',updated_at=now() WHERE schedule_id=?::uuid AND status IN ('scheduled','starting')" (Only scheduleId)
      when (now >= completesAt) $ do
        _ <- execute "UPDATE notification_jobs SET status='pending',run_at=now(),next_attempt_at=NULL,updated_at=now() WHERE schedule_id=?::uuid AND job_type='complete' AND status IN ('pending','retry')" (Only scheduleId)
        pure ()

validUUIDText :: Text -> Bool
validUUIDText value =
  map T.length (T.splitOn "-" value) == [8,4,4,4,12]
    && T.all (\character -> character == '-' || isHexDigit character) value

deleteInstallation :: MonadDB m => String -> Text -> m ()
deleteInstallation username installationId = withTransaction $ do
  owners <- runQuery "SELECT username FROM ios_notification_installations WHERE installation_id=? FOR UPDATE" (Only installationId) :: MonadDB m => m [Only String]
  case owners of
    [] -> pure ()
    Only owner : _ -> do
      ensure (owner == username) (NotFound "Installation not found.")
      _ <- execute "UPDATE notification_jobs SET status='cancelled',updated_at=now() WHERE installation_id=? AND status IN ('pending','retry','running')" (Only installationId)
      _ <- execute
        "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at,target_token,target_environment,immediate_dismissal) SELECT s.schedule_id,s.installation_id,s.generation,'end',now(),a.update_token,a.apns_environment,true FROM streak_notification_schedules s JOIN streak_live_activities a ON a.schedule_id=s.schedule_id WHERE s.installation_id=? AND a.token_valid ON CONFLICT (schedule_id,job_type) DO UPDATE SET status='pending',run_at=now(),target_token=EXCLUDED.target_token,target_environment=EXCLUDED.target_environment,immediate_dismissal=true,updated_at=now()"
        (Only installationId)
      _ <- execute "UPDATE streak_live_activities SET token_valid=false,updated_at=now() WHERE installation_id=?" (Only installationId)
      _ <- execute "UPDATE streak_notification_schedules SET status='superseded',updated_at=now() WHERE installation_id=? AND status IN ('scheduled','starting','active','complete','ending')" (Only installationId)
      _ <- execute "UPDATE ios_notification_installations SET push_to_start_token=NULL,token_updated_at=NULL,live_activities_enabled=false,deleted_at=now(),updated_at=now() WHERE installation_id=?" (Only installationId)
      pure ()
