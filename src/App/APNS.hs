{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App.APNS
  ( APNSClient
  , APNSRequest(..)
  , APNSResponse(..)
  , newAPNSClient
  , invalidateProviderToken
  , sendAPNS
  , tokenHex
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (Value, encode, decode, withObject, (.:?), FromJSON(..))
import qualified Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showHex)
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Process
import System.Exit (ExitCode(..))
import App.Config (APNSConfiguration(..))
import Models.StreakNotification (APNSEnvironment(..))

data CachedJWT = CachedJWT UTCTime BS.ByteString
data APNSClient = APNSClient APNSConfiguration (IORef (Maybe CachedJWT))

data APNSRequest = APNSRequest
  { requestEnvironment :: APNSEnvironment
  , requestToken :: BS.ByteString
  , requestPayload :: Value
  , requestExpiration :: UTCTime
  , requestId :: String
  }

data APNSResponse = APNSResponse
  { responseStatus :: Int
  , responseReason :: Maybe String
  , responseApnsId :: Maybe String
  , responseRetryAfter :: Maybe String
  } deriving (Eq, Show)

newAPNSClient :: APNSConfiguration -> IO APNSClient
newAPNSClient config = APNSClient config <$> newIORef Nothing

invalidateProviderToken :: APNSClient -> IO ()
invalidateProviderToken (APNSClient _ cache) = writeIORef cache Nothing

tokenHex :: BS.ByteString -> String
tokenHex = concatMap twoHex . BS.unpack
  where twoHex byte = let rendered = map toLower (showHex byte "") in replicate (2 - length rendered) '0' ++ rendered

base64Url :: BS.ByteString -> BS.ByteString
base64Url = B64URL.encodeUnpadded

providerJWT :: APNSClient -> IO BS.ByteString
providerJWT (APNSClient config cache) = do
  now <- getCurrentTime
  cached <- readIORef cache
  case cached of
    Just (CachedJWT created token) | diffUTCTime now created < 50 * 60 -> pure token
    _ -> do
      let issuedAt = floor (utcTimeToPOSIXSeconds now) :: Integer
          header = base64Url . LBS.toStrict $ encode (Data.Aeson.object ["alg" Data.Aeson..= ("ES256" :: String), "kid" Data.Aeson..= apnsKeyId config])
          claims = base64Url . LBS.toStrict $ encode (Data.Aeson.object ["iss" Data.Aeson..= apnsTeamId config, "iat" Data.Aeson..= issuedAt])
          signingInput = header <> "." <> claims
      signatureDER <- runOpenSSL (apnsPrivateKeyPath config) signingInput
      signature <- either (ioError . userError) pure (ecdsaDERToRaw signatureDER)
      let token = signingInput <> "." <> base64Url signature
      writeIORef cache (Just (CachedJWT now token))
      pure token

runOpenSSL :: FilePath -> BS.ByteString -> IO BS.ByteString
runOpenSSL keyPath input = do
  (Just stdIn, Just stdOut, Just stdErr, process) <- createProcess
    (proc "openssl" ["dgst", "-sha256", "-sign", keyPath]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  BS.hPut stdIn input
  hClose stdIn
  output <- BS.hGetContents stdOut
  errors <- BS.hGetContents stdErr
  result <- waitForProcess process
  case result of
    ExitSuccess -> pure output
    ExitFailure _ -> ioError . userError $ "Unable to sign APNs provider token: " ++ BS8.unpack errors

ecdsaDERToRaw :: BS.ByteString -> Either String BS.ByteString
ecdsaDERToRaw bytes = do
  (_, sequenceBody) <- takeTLV 0x30 bytes
  (r, remaining) <- takeTLV 0x02 sequenceBody
  (s, trailing) <- takeTLV 0x02 remaining
  when (not (BS.null trailing)) (Left "Unexpected bytes in ECDSA signature")
  (BS.append <$> normalize r <*> normalize s)
  where
    normalize integerBytes =
      let positive = BS.dropWhile (== 0) integerBytes
      in if BS.length positive > 32 then Left "Oversized ECDSA integer" else Right (BS.replicate (32 - BS.length positive) 0 <> positive)
    takeTLV tag input = case BS.uncons input of
      Just (actualTag, rest) | actualTag == tag -> do
        (lengthValue, body) <- decodeLength rest
        if BS.length body < lengthValue then Left "Truncated DER value"
          else Right (BS.take lengthValue body, BS.drop lengthValue body)
      _ -> Left "Invalid ECDSA DER signature"
    decodeLength input = case BS.uncons input of
      Just (value, rest) | value < 128 -> Right (fromIntegral value, rest)
      Just (0x81, rest) -> case BS.uncons rest of Just (value, body) -> Right (fromIntegral value, body); _ -> Left "Truncated DER length"
      _ -> Left "Unsupported DER length"

sendAPNS :: APNSClient -> APNSRequest -> IO APNSResponse
sendAPNS client@(APNSClient config _) request = do
  response <- sendOnce =<< providerJWT client
  if responseStatus response == 403 && responseReason response == Just "ExpiredProviderToken"
    then invalidateProviderToken client >> (sendOnce =<< providerJWT client)
    else pure response
  where
    sendOnce jwt = withPayloadFile (LBS.toStrict $ encode request.requestPayload) $ \payloadPath -> do
      let host = case request.requestEnvironment of Sandbox -> "https://api.sandbox.push.apple.com"; Production -> "https://api.push.apple.com"
          expiry = show (floor (utcTimeToPOSIXSeconds request.requestExpiration) :: Integer)
          curlConfig = unlines
            [ "url = \"" ++ host ++ "/3/device/" ++ tokenHex request.requestToken ++ "\""
            , "request = \"POST\""
            , "header = \"authorization: bearer " ++ BS8.unpack jwt ++ "\""
            , "header = \"apns-push-type: liveactivity\""
            , "header = \"apns-topic: " ++ apnsBundleId config ++ ".push-type.liveactivity\""
            , "header = \"apns-priority: 10\""
            , "header = \"content-type: application/json\""
            , "header = \"apns-expiration: " ++ expiry ++ "\""
            , "header = \"apns-id: " ++ request.requestId ++ "\""
            , "data-binary = \"@" ++ payloadPath ++ "\""
            ]
      (exitCode, output, errors) <- readCreateProcessWithExitCode
        (proc "curl" ["--http2", "--silent", "--show-error", "--include", "--write-out", "\n%{http_code}", "--config", "-"])
        curlConfig
      case exitCode of
        ExitFailure _ -> ioError . userError $ "APNs transport failed: " ++ take 512 errors
        ExitSuccess -> pure (parseCurlResponse output)

withPayloadFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withPayloadFile payload = bracket acquire removeFile
  where
    acquire = do
      (path, handle) <- openBinaryTempFile "/tmp" "nimzo-apns-payload.json"
      BS.hPut handle payload
      hClose handle
      pure path

newtype ReasonBody = ReasonBody { reason :: Maybe String }
instance FromJSON ReasonBody where parseJSON = withObject "APNs error" $ \o -> ReasonBody <$> o .:? "reason"

parseCurlResponse :: String -> APNSResponse
parseCurlResponse output =
  let outputLines = lines output
      status = case reverse outputLines of value : _ -> fromMaybe 0 (readMaybe value); _ -> 0
      header name = fmap (dropWhile (== ' ') . drop (length name + 1)) . find (isPrefixOf (name ++ ":")) $ outputLines
      bodyLine = find (isPrefixOf "{") outputLines
      parsedReason = bodyLine >>= (decode . LBS.fromStrict . BS8.pack) >>= reason
  in APNSResponse status parsedReason (header "apns-id") (header "retry-after")
  where readMaybe value = case reads value of [(number, "")] -> Just number; _ -> Nothing
