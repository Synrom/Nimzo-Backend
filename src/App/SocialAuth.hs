{-# LANGUAGE OverloadedStrings #-}

module App.SocialAuth where

import Control.Monad.Except (Except, runExcept)
import Data.Aeson (Value(..), eitherDecode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Crypto.JWT
import Crypto.JOSE.JWK (JWKSet)
import App.Config (SocialAuthConfiguration(..))
import App.Error (AppError(..))
import Models.SocialAuth (SocialProfile(..))

data ProviderSettings = ProviderSettings
  { clientIds :: [String],
    issuers :: [String],
    jwksUrl :: String
  }

invalidTokenError :: AppError
invalidTokenError = Unauthorized "Invalid social login token."

providerNotConfigured :: AppError
providerNotConfigured = Internal "Google login is not configured."

providerSettings :: SocialAuthConfiguration -> ProviderSettings
providerSettings cfg =
  ProviderSettings (googleClientIds cfg) ["accounts.google.com", "https://accounts.google.com"] "https://www.googleapis.com/oauth2/v3/certs"

verifyGoogleToken :: SocialAuthConfiguration -> String -> IO (Either AppError SocialProfile)
verifyGoogleToken cfg token
  | null (clientIds settings) = do
      return $ Left providerNotConfigured
  | otherwise = do
      putStrLn $ "[SocialAuth] verifying Google token len=" ++ show (length token)
      putStrLn $ "[SocialAuth] allowed clients: " ++ show (clientIds settings)
      putStrLn $ "[SocialAuth] allowed issuers: " ++ show (issuers settings)
      manager <- getGlobalManager
      request <- parseRequest (jwksUrl settings)
      response <- httpLbs request manager
      now <- getCurrentTime
      case decodeJwkSet (responseBody response) of
        Left err -> do
          putStrLn $ "[SocialAuth] Failed to decode Google JWK set: " ++ show err
          return $ Left err
        Right jwkSet -> do
          eitherClaims <- decodeAndVerifyClaims "Google" settings now jwkSet token
          case eitherClaims of
            Left err -> return $ Left err
            Right claims -> return $ claimsToProfile claims
  where
    settings = providerSettings cfg

decodeJwkSet :: BL.ByteString -> Either AppError JWKSet
decodeJwkSet body = case eitherDecode body of
  Left _ -> Left invalidTokenError
  Right jwkSet -> Right jwkSet

decodeAndVerifyClaims :: String -> ProviderSettings -> UTCTime -> JWKSet -> String -> IO (Either AppError ClaimsSet)
decodeAndVerifyClaims providerName settings now jwkSet token = case runExcept verification of
  Left jwtErr -> do
    putStrLn $ "[SocialAuth] JWT verification failed for " ++ providerName ++ ": " ++ show jwtErr
    return $ Left invalidTokenError
  Right claims
    | validIssuer claims -> return $ Right claims
    | otherwise -> do
        putStrLn $ "[SocialAuth] Invalid issuer for " ++ providerName ++ ": " ++ show (claimString "iss" claims)
        return $ Left invalidTokenError
  where
    verification :: Except JWTError ClaimsSet
    verification = do
      let validationSettings = defaultJWTValidationSettings audiencePredicate
      signedJwt <- decodeCompact (BL.fromStrict $ T.encodeUtf8 $ T.pack token)
      verifyClaimsAt validationSettings jwkSet now signedJwt
    audiencePredicate audience = any (`matchesStringOrUri` audience) (clientIds settings)
    validIssuer claims = maybe False (\issuer -> issuer `elem` issuers settings) (claimString "iss" claims)

claimsToProfile :: ClaimsSet -> Either AppError SocialProfile
claimsToProfile claims = case claimObject claims of
  Nothing -> Left invalidTokenError
  Just object -> do
    subject <- maybe (Left invalidTokenError) Right (objectString "sub" object)
    Right $
      SocialProfile
        subject
        (objectString "email" object)
        (maybe False id $ objectBoolish "email_verified" object)

claimObject :: ClaimsSet -> Maybe (KeyMap.KeyMap Value)
claimObject claims = case toJSON claims of
  Object object -> Just object
  _ -> Nothing

claimString :: T.Text -> ClaimsSet -> Maybe String
claimString key claims = claimObject claims >>= objectString key

objectString :: T.Text -> KeyMap.KeyMap Value -> Maybe String
objectString key object = case KeyMap.lookup (Key.fromText key) object of
  Just (String value) -> Just (T.unpack value)
  _ -> Nothing

objectBoolish :: T.Text -> KeyMap.KeyMap Value -> Maybe Bool
objectBoolish key object = case KeyMap.lookup (Key.fromText key) object of
  Just (Bool value) -> Just value
  Just (String "true") -> Just True
  Just (String "false") -> Just False
  _ -> Nothing

matchesStringOrUri :: String -> StringOrURI -> Bool
matchesStringOrUri expected value = fromStringOrUri value == expected

fromStringOrUri :: StringOrURI -> String
fromStringOrUri value = case toJSON value of
  String text -> T.unpack text
  _ -> show value
