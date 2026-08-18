{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker.StreakNotification
  ( runWorker
  , runOnce
  , runOnceWith
  , startPayload
  , completePayload
  , endPayload
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, forM_, when)
import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.Types (Binary(..))
import Numeric (showHex)
import System.Random (randomIO)
import App.APNS
import Models.StreakNotification (APNSEnvironment(..))

data ClaimedJob = ClaimedJob Text
instance FromRow ClaimedJob where fromRow = ClaimedJob <$> field

data ReadyJob = ReadyJob
  { jobId :: Text
  , scheduleId :: Text
  , installationId :: Text
  , generation :: Integer
  , jobType :: Text
  , attempts :: Int
  , startsAt :: UTCTime
  , completesAt :: UTCTime
  , scheduleStatus :: Text
  , osVersion :: Text
  , installationEnvironment :: APNSEnvironment
  , pushToken :: Maybe ByteString
  , activityToken :: Maybe ByteString
  , activityEnvironment :: Maybe APNSEnvironment
  , targetToken :: Maybe ByteString
  , targetEnvironment :: Maybe APNSEnvironment
  , immediateDismissal :: Bool
  }

instance FromRow ReadyJob where
  fromRow = ReadyJob <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> (environment <$> field) <*> (unBinary <$> field) <*> (unBinary <$> field) <*> (fmap environment <$> field)
    <*> (unBinary <$> field) <*> (fmap environment <$> field) <*> field
    where
      environment (value :: Text) = if value == "sandbox" then Sandbox else Production
      unBinary = fmap (\(Binary bytes) -> bytes)

runWorker :: Connection -> APNSClient -> String -> IO ()
runWorker connection client workerId = forever $ do
  count <- runOnce connection client workerId
  when (count == 0) (threadDelay 1000000)

runOnce :: Connection -> APNSClient -> String -> IO Int
runOnce connection client = runOnceWith connection (sendAPNS client)

runOnceWith :: Connection -> (APNSRequest -> IO APNSResponse) -> String -> IO Int
runOnceWith connection send workerId = do
  recoverCrashedJobs connection
  claimed <- claimJobs connection workerId
  forM_ claimed $ \(ClaimedJob identifier) -> do
    result <- try (processJob connection send identifier) :: IO (Either SomeException ())
    case result of
      Left exception -> retryNetworkFailure connection identifier (show exception)
      Right () -> pure ()
  pure (length claimed)

recoverCrashedJobs :: Connection -> IO ()
recoverCrashedJobs connection = do
  _ <- execute_ connection "UPDATE notification_jobs SET status='retry',locked_at=NULL,locked_by=NULL,next_attempt_at=now(),last_error='worker lock expired',updated_at=now() WHERE status='running' AND locked_at < now() - interval '5 minutes'"
  pure ()

claimJobs :: Connection -> String -> IO [ClaimedJob]
claimJobs connection workerId = withTransaction connection $ query connection
  "WITH candidate AS (SELECT job_id FROM notification_jobs WHERE status IN ('pending','retry') AND COALESCE(next_attempt_at,run_at)<=now() ORDER BY COALESCE(next_attempt_at,run_at) FOR UPDATE SKIP LOCKED LIMIT 20) UPDATE notification_jobs j SET status='running',locked_at=now(),locked_by=?,attempts=attempts+1,updated_at=now() FROM candidate WHERE j.job_id=candidate.job_id RETURNING j.job_id::text"
  (Only workerId)

loadReadyJob :: Connection -> Text -> IO (Maybe ReadyJob)
loadReadyJob connection identifier = do
  rows <- query connection
    "SELECT j.job_id::text,s.schedule_id::text,j.installation_id,j.generation,j.job_type,j.attempts,s.starts_at,s.completes_at,s.status,i.os_version,i.apns_environment,i.push_to_start_token,a.update_token,a.apns_environment,j.target_token,j.target_environment,j.immediate_dismissal FROM notification_jobs j JOIN streak_notification_schedules s ON s.schedule_id=j.schedule_id JOIN ios_notification_installations i ON i.installation_id=j.installation_id LEFT JOIN streak_live_activities a ON a.schedule_id=s.schedule_id AND a.token_valid WHERE j.job_id=?::uuid AND j.status='running'"
    (Only identifier)
  pure $ case rows of row : _ -> Just row; _ -> Nothing

processJob :: Connection -> (APNSRequest -> IO APNSResponse) -> Text -> IO ()
processJob connection send identifier = do
  maybeJob <- loadReadyJob connection identifier
  case maybeJob of
    Nothing -> pure ()
    Just job
      | job.jobType /= "end" && job.scheduleStatus `elem` ["superseded", "ended", "failed"] -> cancelJob connection job.jobId "stale schedule"
      | otherwise -> deliver connection send job

deliver :: Connection -> (APNSRequest -> IO APNSResponse) -> ReadyJob -> IO ()
deliver connection send job = do
  now <- getCurrentTime
  case tokenForJob job of
    Left missingReason
      | job.jobType == "complete" && now <= addUTCTime 600 job.completesAt -> retryAfter connection job.jobId 30 missingReason
      | job.jobType == "end" -> succeed connection job
      | otherwise -> dead connection job missingReason Nothing
    Right (token, environment)
      | environment /= job.installationEnvironment && not (job.jobType == "end" && maybe False (const True) job.targetToken) -> dead connection job "APNs environment mismatch" Nothing
      | otherwise -> do
          apnsId <- randomUUID
          let expiration = case job.jobType of
                "start" -> addUTCTime 900 job.startsAt
                "complete" -> addUTCTime 3600 job.completesAt
                _ -> addUTCTime (24 * 3600) now
              payload = case job.jobType of
                "start" -> startPayload now job.scheduleId job.generation job.startsAt job.completesAt (iosAtLeast18 job.osVersion)
                "complete" -> completePayload now job.completesAt
                _ -> endPayload now job.completesAt job.immediateDismissal
          if expiration <= now
            then dead connection job "APNs expiration passed" Nothing
            else do
              response <- send (APNSRequest environment token payload expiration apnsId)
              handleResponse connection job response

tokenForJob :: ReadyJob -> Either String (ByteString, APNSEnvironment)
tokenForJob job = case job.jobType of
  "start" -> maybe (Left "missing push-to-start token") (\token -> Right (token, job.installationEnvironment)) job.pushToken
  "complete" -> case (job.activityToken, job.activityEnvironment) of
    (Just token, Just environment) -> Right (token, environment)
    _ -> Left "missing activity update token"
  _ -> case (job.targetToken, job.targetEnvironment) of
    (Just token, Just environment) -> Right (token, environment)
    _ -> case (job.activityToken, job.activityEnvironment) of
      (Just token, Just environment) -> Right (token, environment)
      _ -> Left "missing activity update token"

handleResponse :: Connection -> ReadyJob -> APNSResponse -> IO ()
handleResponse connection job response
  | response.responseStatus == 200 = succeedWithResponse connection job response
  | response.responseStatus `elem` [429, 500, 503] = transient
  | response.responseStatus == 410 || (response.responseStatus == 400 && response.responseReason == Just "BadDeviceToken") = do
      invalidateTargetToken connection job
      if job.jobType == "end" then succeedWithResponse connection job response else dead connection job "APNs token is invalid" (Just response)
  | otherwise = dead connection job (fromMaybe "APNs permanent failure" response.responseReason) (Just response)
  where
    transient = retryAfter connection job.jobId (retryDelay job.attempts) (fromMaybe "transient APNs failure" response.responseReason)

retryDelay :: Int -> NominalDiffTime
retryDelay attempt = case attempt of 1 -> 5; 2 -> 30; 3 -> 120; 4 -> 600; _ -> 1800

succeedWithResponse :: Connection -> ReadyJob -> APNSResponse -> IO ()
succeedWithResponse connection job response = withTransaction connection $ do
  _ <- execute connection "UPDATE notification_jobs SET status='succeeded',locked_at=NULL,locked_by=NULL,last_apns_status=?,last_apns_reason=?,last_apns_id=?,updated_at=now() WHERE job_id=?::uuid AND status='running'"
    (response.responseStatus, response.responseReason, response.responseApnsId, job.jobId)
  updateScheduleAfterSuccess connection job

succeed :: Connection -> ReadyJob -> IO ()
succeed connection job = withTransaction connection $ do
  _ <- execute connection "UPDATE notification_jobs SET status='succeeded',locked_at=NULL,locked_by=NULL,updated_at=now() WHERE job_id=?::uuid AND status='running'" (Only job.jobId)
  updateScheduleAfterSuccess connection job

updateScheduleAfterSuccess :: Connection -> ReadyJob -> IO ()
updateScheduleAfterSuccess connection job = case job.jobType of
  "start" -> do
    _ <- execute connection "UPDATE streak_notification_schedules SET status='starting',updated_at=now() WHERE schedule_id=?::uuid AND generation=? AND status='scheduled'" (job.scheduleId, job.generation)
    pure ()
  "complete" -> do
    _ <- execute connection "UPDATE streak_notification_schedules SET status='complete',updated_at=now() WHERE schedule_id=?::uuid AND generation=? AND status NOT IN ('superseded','ended')" (job.scheduleId, job.generation)
    _ <- execute connection "INSERT INTO notification_jobs (schedule_id,installation_id,generation,job_type,run_at) VALUES (?::uuid,?,?,'end',now()+interval '2 hours') ON CONFLICT (schedule_id,job_type) DO NOTHING" (job.scheduleId, job.installationId, job.generation)
    pure ()
  _ -> do
    _ <- execute connection "UPDATE streak_notification_schedules SET status='ended',updated_at=now() WHERE schedule_id=?::uuid" (Only job.scheduleId)
    _ <- execute connection "UPDATE streak_live_activities SET token_valid=false,updated_at=now() WHERE schedule_id=?::uuid" (Only job.scheduleId)
    pure ()

cancelJob :: Connection -> Text -> String -> IO ()
cancelJob connection identifier message = do
  _ <- execute connection "UPDATE notification_jobs SET status='cancelled',locked_at=NULL,locked_by=NULL,last_error=?,updated_at=now() WHERE job_id=?::uuid" (message, identifier)
  pure ()

dead :: Connection -> ReadyJob -> String -> Maybe APNSResponse -> IO ()
dead connection job message response = do
  _ <- execute connection "UPDATE notification_jobs SET status='dead',locked_at=NULL,locked_by=NULL,last_error=?,last_apns_status=?,last_apns_reason=?,last_apns_id=?,updated_at=now() WHERE job_id=?::uuid"
    (message, responseStatus <$> response, response >>= responseReason, response >>= responseApnsId, job.jobId)
  when (job.jobType /= "end") $ do
    _ <- execute connection "UPDATE streak_notification_schedules SET status='failed',updated_at=now() WHERE schedule_id=?::uuid AND status NOT IN ('superseded','ended')" (Only job.scheduleId)
    pure ()

retryAfter :: Connection -> Text -> NominalDiffTime -> String -> IO ()
retryAfter connection identifier delay message = do
  _ <- execute connection "UPDATE notification_jobs SET status='retry',locked_at=NULL,locked_by=NULL,next_attempt_at=now()+(? * interval '1 second'),last_error=?,updated_at=now() WHERE job_id=?::uuid" (realToFrac delay :: Double, message, identifier)
  pure ()

retryNetworkFailure :: Connection -> Text -> String -> IO ()
retryNetworkFailure connection identifier message = retryAfter connection identifier 30 (take 512 message)

invalidateTargetToken :: Connection -> ReadyJob -> IO ()
invalidateTargetToken connection job
  | job.jobType == "start" = do
      _ <- execute connection "UPDATE ios_notification_installations SET push_to_start_token=NULL,token_updated_at=NULL,updated_at=now() WHERE installation_id=?" (Only job.installationId)
      pure ()
  | otherwise = do
      _ <- execute connection "UPDATE streak_live_activities SET token_valid=false,updated_at=now() WHERE schedule_id=?::uuid" (Only job.scheduleId)
      pure ()

unixSeconds :: UTCTime -> Integer
unixSeconds = floor . utcTimeToPOSIXSeconds

contentState :: UTCTime -> Bool -> Value
contentState completes isComplete = object
  [ "endsAtTimestamp" .= unixSeconds completes
  , "title" .= ("Last chance!" :: String)
  , "completionTitle" .= ("Nimzo misses you!" :: String)
  , "completionBody" .= ("Got time for studying?" :: String)
  , "isComplete" .= isComplete
  ]

startPayload :: UTCTime -> Text -> Integer -> UTCTime -> UTCTime -> Bool -> Value
startPayload now schedule generation starts completes requestInputToken = object ["aps" .= object
  ([ "timestamp" .= unixSeconds now
   , "event" .= ("start" :: String)
   , "content-state" .= contentState completes False
   , "attributes-type" .= ("StreakActivityAttributes" :: String)
   , "attributes" .= object ["scheduleId" .= schedule, "generation" .= generation, "startsAtTimestamp" .= unixSeconds starts]
   , "stale-date" .= unixSeconds completes
   , "relevance-score" .= (100 :: Int)
   , "alert" .= object ["title" .= ("Last chance!" :: String), "body" .= ("Your streak countdown has started." :: String), "sound" .= ("default" :: String)]
   ] ++ ["input-push-token" .= (1 :: Int) | requestInputToken])]

completePayload :: UTCTime -> UTCTime -> Value
completePayload now completes = object ["aps" .= object
  [ "timestamp" .= unixSeconds now
  , "event" .= ("update" :: String)
  , "content-state" .= contentState completes True
  , "relevance-score" .= (100 :: Int)
  , "alert" .= object ["title" .= ("Nimzo misses you!" :: String), "body" .= ("Got time for studying?" :: String), "sound" .= ("default" :: String)]
  ]]

endPayload :: UTCTime -> UTCTime -> Bool -> Value
endPayload now completes immediate = object ["aps" .= object
  [ "timestamp" .= unixSeconds now
  , "event" .= ("end" :: String)
  , "dismissal-date" .= unixSeconds (if immediate then addUTCTime (-1) now else addUTCTime (4 * 3600) now)
  , "content-state" .= contentState completes True
  ]]

iosAtLeast18 :: Text -> Bool
iosAtLeast18 raw = case reads (T.unpack $ T.takeWhile (/= '.') raw) of [(major, _)] -> (major :: Int) >= 18; _ -> False

randomUUID :: IO String
randomUUID = do
  a <- randomIO :: IO Word64
  b <- randomIO :: IO Word64
  let digits = hex16 a ++ hex16 b
  pure $ take 8 digits ++ "-" ++ take 4 (drop 8 digits) ++ "-4" ++ take 3 (drop 13 digits) ++ "-a" ++ take 3 (drop 17 digits) ++ "-" ++ take 12 (drop 20 digits)
  where hex16 value = let rendered = showHex value "" in replicate (16 - length rendered) '0' ++ rendered
