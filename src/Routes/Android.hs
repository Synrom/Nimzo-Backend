{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Routes.Android where

import Data.List (dropWhileEnd)
import Servant (JSON, Post, ReqBody, type (:>))
import App.AppM
import App.Error (AppError(..))
import App.Mail (sendAndroidEmailNotification)
import App.Async (forkAppM)
import Models.AndroidEmail (AndroidEmailPayload(..), androidEmailMaxLength)
import Models.Watermelon (JsonableMsg(..))
import Repo.Utils (ensure)
import qualified Repo.AndroidEmail as AndroidEmailRepo

type API =
  "android" :> "emails" :> ReqBody '[JSON] AndroidEmailPayload :> Post '[JSON] JsonableMsg

type Server =
  AndroidEmailPayload -> AppM JsonableMsg

invalidAndroidEmail :: AppError
invalidAndroidEmail = Unauthorized "Invalid email."

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

saveAndroidEmail :: AndroidEmailPayload -> AppM JsonableMsg
saveAndroidEmail payload = do
  let normalizedEmail = trim payload.email
  ensure invalidAndroidEmail (not (null normalizedEmail))
  ensure invalidAndroidEmail (length normalizedEmail <= androidEmailMaxLength)
  _ <- AndroidEmailRepo.insert $ AndroidEmailPayload normalizedEmail
  forkAppM $ sendAndroidEmailNotification normalizedEmail
  pure $ Msg "Successfully saved android email."

server :: Server
server = saveAndroidEmail
