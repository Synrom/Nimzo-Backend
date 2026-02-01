{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Mail where

import Configuration.Dotenv (load, parseFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Network.Mail.Mime (htmlPart)
import Network.Mail.SMTP (Address (Address), sendMailWithLoginTLS, simpleMail)
import qualified System.Environment as Env
import Repo.Classes
import App.AppM (AppM(..))
import App.Env (Env(..))
import App.Config (MailConfiguration(..))
import Control.Monad (when, unless)

instance MonadMail AppM where
  sendMail mail = do
    env <- askEnv
    let auth = mailConfig env
    liftIO $ sendMailWithLoginTLS "smtp.gmail.com" auth.username auth.password mail
  mailCfg = do
    env <- askEnv
    return env.mailConfig


sendVerificationMail :: MonadMail m => String -> String -> String -> m ()
sendVerificationMail user_name email verification_token = do
  auth <- mailCfg
  unless auth.test $ do
    let
      from_address = Address (Just auth.name) auth.mail
      to_address = [Address (Just $ T.pack user_name) $ T.pack email]
      cc = []
      bcc = []
      subject = "Please Confirm Your Nimzo Account"
      body =
        htmlPart $
          TL.pack $
            "<!DOCTYPE html> \
            \ <html language=\"en\"> \
            \ <head> \
            \ <meta charset=\"UTF-8\"> \
            \ <title>Verification Email</title> \
            \ <style>            \
            \ body {         \
            \ font-family: Arial, sans-serif;\
            \ margin: 0; \
            \ padding: 0;\
            \ color: #333;\
            \ }              \
            \ \
            \ .wrapper {     \
            \ width: 90%;\
            \ max-width: 600px;\
            \ margin: 0 auto;\
            \ padding: 40px 0;\
            \ text-align: center;\
            \ }              \
            \ \
            \ .button {      \
            \ display: inline-block;\
            \ color: #fff;\
            \ background-color: #007bff;\
            \ border-color: #007bff;\
            \ padding: .375rem .75rem;\
            \ font-size: 1rem;\
            \ border-radius: .25rem;\
            \ cursor: pointer;\
            \ text-decoration: none;\
            \ }              \
            \ \
            \ .button:hover {\
            \ background-color: #0056b3;\
            \ }              \
            \ \
            \ .footer {      \
            \ margin-top: 40px;\
            \ font-size: .875rem;\
            \ color: #6c757d;\
            \ }              \
            \ </style>           \
            \ </head>                \
            \ <body>                 \
            \ <div class=\"wrapper\">\
            \ <h1>Welcome to Nimzo</h1>\
            \ <p>Dear "
              ++ user_name
              ++ ",</p>\
                \ <p>Thank you for registering on the Nimzo App. Please verify your email to complete your registration</p>\
                \ <a href=\""
              ++ auth.verification_link
              ++ "?token="
              ++ verification_token
              ++ "\" class=\"button\">Verify your email</a>\
                \ <p class=\"footer\">If you didn't make this request, please ignore this email.</p>\
                \ </div>             \
                \ </body>                \
                \ </html>"
      veritication_mail = simpleMail from_address to_address cc bcc subject [body]
    sendMail veritication_mail

sendChangePasswordMail :: MonadMail m => String -> String -> String -> m () 
sendChangePasswordMail user_name email access_token = do
  auth <- mailCfg
  unless auth.test $ do
    let
      from_address = Address (Just auth.name) auth.mail
      to_address = [Address (Just $ T.pack user_name) $ T.pack email]
      cc = []
      bcc = []
      subject = "Reset Your Nimzo Password"
      body =
        htmlPart $
          TL.pack $
            "<!DOCTYPE html> \
            \ <html lang=\"en\"> \
            \ <head> \
            \ <meta charset=\"UTF-8\"> \
            \ <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"> \
            \ <title>Password Reset</title> \
            \ <style> \
            \ body { \
            \   font-family: Arial, sans-serif; \
            \   margin: 0; \
            \   padding: 0; \
            \   color: #333; \
            \ } \
            \ .wrapper { \
            \   width: 90%; \
            \   max-width: 600px; \
            \   margin: 0 auto; \
            \   padding: 40px 0; \
            \   text-align: center; \
            \ } \
            \ .button { \
            \   display: inline-block; \
            \   color: #fff; \
            \   background-color: #007bff; \
            \   border-color: #007bff; \
            \   padding: .375rem .75rem; \
            \   font-size: 1rem; \
            \   border-radius: .25rem; \
            \   cursor: pointer; \
            \   text-decoration: none; \
            \ } \
            \ .button:hover { \
            \   background-color: #0056b3; \
            \ } \
            \ .muted { \
            \   margin-top: 16px; \
            \   font-size: .95rem; \
            \   color: #6c757d; \
            \ } \
            \ .footer { \
            \   margin-top: 40px; \
            \   font-size: .875rem; \
            \   color: #6c757d; \
            \ } \
            \ .linkbox { \
            \   margin-top: 18px; \
            \   padding: 12px; \
            \   border: 1px solid #e9ecef; \
            \   border-radius: .25rem; \
            \   word-break: break-all; \
            \   text-align: left; \
            \   font-size: .95rem; \
            \ } \
            \ </style> \
            \ </head> \
            \ <body> \
            \ <div class=\"wrapper\"> \
            \   <h1>Nimzo Password Reset</h1> \
            \   <p>Dear "
              ++ user_name
              ++ ",</p> \
            \   <p>We received a request to reset your Nimzo password. Click the button below to choose a new one.</p> \
            \   <a href=\""
              ++ auth.change_pwd_link 
              ++ "?token="
              ++ access_token
              ++ "\" class=\"button\">Change your password</a> \
            \   <p class=\"muted\">This link will expire soon for your security.</p> \
            \   <div class=\"linkbox\"> \
            \     <strong>If the button doesn’t work, copy and paste this link into your browser:</strong><br/> \
            \     "
              ++ auth.change_pwd_link 
              ++ "?token="
              ++ access_token
              ++ " \
            \   </div> \
            \   <p class=\"footer\">If you didn't make this request, you can safely ignore this email.</p> \
            \ </div> \
            \ </body> \
            \ </html>"
      veritication_mail = simpleMail from_address to_address cc bcc subject [body]
    sendMail veritication_mail
