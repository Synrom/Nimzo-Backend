{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Routes.StreakNotification where

import Data.Text (Text)
import Servant (Capture, JSON, NoContent(..), Post, Put, ReqBody, StdMethod(DELETE, PUT), Verb, type (:<|>)(..), type (:>))
import Servant.Auth.Server (AuthResult(..))
import App.AppM
import App.Auth (AuthenticatedUser(..))
import App.Error (AppError(..), throwAppError)
import Models.StreakNotification
import qualified Repo.StreakNotification as Repo

type API =
       "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> ReqBody '[JSON] UpsertIOSInstallationRequest
         :> Put '[JSON] IOSInstallationResponse
  :<|> "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> "activitykit" :> "push-to-start-token"
         :> ReqBody '[JSON] PushToStartTokenRequest
         :> Verb 'PUT 204 '[JSON] NoContent
  :<|> "notifications" :> "streak" :> "card-played"
         :> ReqBody '[JSON] CardPlayedRequest
         :> Post '[JSON] StreakScheduleResponse
  :<|> "notifications" :> "streak" :> "schedules"
         :> Capture "scheduleId" Text
         :> "activity-token"
         :> ReqBody '[JSON] ActivityTokenRequest
         :> Verb 'PUT 204 '[JSON] NoContent
  :<|> "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> Verb 'DELETE 204 '[JSON] NoContent

type StreakServer =
       (Text -> UpsertIOSInstallationRequest -> AppM IOSInstallationResponse)
  :<|> (Text -> PushToStartTokenRequest -> AppM NoContent)
  :<|> (CardPlayedRequest -> AppM StreakScheduleResponse)
  :<|> (Text -> ActivityTokenRequest -> AppM NoContent)
  :<|> (Text -> AppM NoContent)

server :: AuthResult AuthenticatedUser -> StreakServer
server (Authenticated user) =
       Repo.upsertInstallation user.username
  :<|> (\installationId request -> Repo.putPushToStartToken user.username installationId request >> pure NoContent)
  :<|> Repo.recordCardPlayed user.username
  :<|> (\scheduleId request -> Repo.putActivityToken user.username scheduleId request >> pure NoContent)
  :<|> (\installationId -> Repo.deleteInstallation user.username installationId >> pure NoContent)
server _ = throwAppError $ Unauthorized "No access."
