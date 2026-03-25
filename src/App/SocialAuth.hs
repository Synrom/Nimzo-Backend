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
import Models.SocialAuth (SocialProfile(..), SocialProvider(..))

data ProviderSettings = ProviderSettings
  { clientIds :: [String],
    issuers :: [String],
    jwksUrl :: String
  }

invalidTokenError :: AppError
invalidTokenError = Unauthorized "Invalid social login token."

providerNotConfigured :: SocialProvider -> AppError
providerNotConfigured provider =
  Internal $ socialProviderName provider ++ " login is not configured."

socialProviderName :: SocialProvider -> String
socialProviderName Google = "Google"
socialProviderName Apple = "Apple"

providerSettings :: SocialAuthConfiguration -> SocialProvider -> ProviderSettings
providerSettings cfg Google =
  ProviderSettings (googleClientIds cfg) ["accounts.google.com", "https://accounts.google.com"] "https://www.googleapis.com/oauth2/v3/certs"
providerSettings cfg Apple =
  ProviderSettings (appleClientIds cfg) ["https://appleid.apple.com"] "https://appleid.apple.com/auth/keys"

verifySocialToken :: SocialAuthConfiguration -> SocialProvider -> String -> IO (Either AppError SocialProfile)
verifySocialToken cfg provider token
  | null (clientIds settings) = pure $ Left $ providerNotConfigured provider
  | otherwise = do
      manager <- getGlobalManager
      request <- parseRequest (jwksUrl settings)
      response <- httpLbs request manager
      now <- getCurrentTime
      pure $ do
        jwkSet <- decodeJwkSet (responseBody response)
        claims <- decodeAndVerifyClaims settings now jwkSet token
        claimsToProfile claims
  where
    settings = providerSettings cfg provider

decodeJwkSet :: BL.ByteString -> Either AppError JWKSet
decodeJwkSet body = case eitherDecode body of
  Left _ -> Left invalidTokenError
  Right jwkSet -> Right jwkSet

decodeAndVerifyClaims :: ProviderSettings -> UTCTime -> JWKSet -> String -> Either AppError ClaimsSet
decodeAndVerifyClaims settings now jwkSet token = case runExcept verification of
  Left _ -> Left invalidTokenError
  Right claims
    | validIssuer claims -> Right claims
    | otherwise -> Left invalidTokenError
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
