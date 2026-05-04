{-# LANGUAGE OverloadedStrings #-}

module Repo.Deck.Validation
  ( maxDeckImageBytes,
    normalizeNullableString,
    normalizePublicBase,
    normalizeFeaturedSource,
    normalizeFeaturedLimit,
    validateDeckImagePayload,
    validateFeaturedSourceStrict,
    resolveFeaturedSource,
    validateFeaturedCardIdRequired,
    validateFeaturedRank,
    validateVideoUrl,
    normalizeUsername,
  )
where

import App.Error (AppError (Unauthorized))
import Data.Char (isSpace, toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.String.HT (trim)
import Models.DeckImage (DeckImageUploadRequest (..))
import Models.FeaturedSource (FeaturedSource (..), parseFeaturedSource)
import Network.URI (parseURI, uriScheme)

maxDeckImageBytes :: Int
maxDeckImageBytes = 5 * 1024 * 1024

normalizeNullableString :: Maybe String -> Maybe String
normalizeNullableString maybeRaw = case fmap trim maybeRaw of
  Just "" -> Nothing
  other -> other

normalizeUsername :: String -> String
normalizeUsername = map toLower . trim

normalizePublicBase :: String -> String
normalizePublicBase raw = case reverse raw of
  '/':rest -> reverse rest
  _ -> raw

normalizeFeaturedSource :: Maybe String -> Maybe String
normalizeFeaturedSource = fmap (map toLower . trim) . normalizeNullableString

normalizeFeaturedSourceTyped :: Maybe String -> Maybe FeaturedSource
normalizeFeaturedSourceTyped maybeRaw = normalizeFeaturedSource maybeRaw >>= parseFeaturedSource

normalizeFeaturedLimit :: Maybe Integer -> Integer
normalizeFeaturedLimit maybeLimit =
  let requested = fromMaybe 20 maybeLimit
   in max 1 (min 50 requested)

validateFeaturedSourceStrict :: Maybe String -> Either AppError (Maybe FeaturedSource)
validateFeaturedSourceStrict maybeRaw = case normalizeFeaturedSourceTyped maybeRaw of
  Nothing
    | normalizeNullableString maybeRaw == Nothing -> Right Nothing
    | otherwise -> Left $ Unauthorized "Invalid featured source. Use 'tiktok', 'instagram', 'youtube', 'x' or null."
  Just source -> Right (Just source)

resolveFeaturedSource :: Maybe String -> Either AppError FeaturedSource
resolveFeaturedSource maybeRaw = case normalizeFeaturedSourceTyped maybeRaw of
  Nothing
    | normalizeNullableString maybeRaw == Nothing -> Right FeaturedSourceTikTok
    | otherwise -> Left $ Unauthorized "Invalid featured source. Use 'tiktok', 'instagram', 'youtube', or 'x'."
  Just source -> Right source

validateFeaturedCardIdRequired :: Maybe String -> Either AppError String
validateFeaturedCardIdRequired maybeRaw = case normalizeNullableString maybeRaw of
  Just cardId -> Right cardId
  Nothing -> Left $ Unauthorized "featuredCardId is required when featuredSource is set."

validateFeaturedRank :: Maybe Integer -> Either AppError (Maybe Integer)
validateFeaturedRank Nothing = Right Nothing
validateFeaturedRank (Just rankValue)
  | rankValue >= 0 && rankValue <= 100000 = Right (Just rankValue)
  | otherwise = Left $ Unauthorized "Invalid featured rank. Use a value between 0 and 100000."

validateVideoUrl :: Maybe String -> Either AppError (Maybe String)
validateVideoUrl maybeRaw = case normalizeNullableString maybeRaw of
  Nothing -> Right Nothing
  Just url
    | length url > 1000 -> Left invalidVideo
    | isValidAbsoluteHttpUrl url -> Right (Just url)
    | otherwise -> Left invalidVideo
  where
    invalidVideo = Unauthorized "Invalid video URL. Use an absolute http(s) URL up to 1000 chars."

isValidAbsoluteHttpUrl :: String -> Bool
isValidAbsoluteHttpUrl raw = case parseURI raw of
  Nothing -> False
  Just uri ->
    let scheme = map toLower (uriScheme uri)
     in scheme == "http:" || scheme == "https:"

validateDeckImagePayload :: DeckImageUploadRequest -> Either AppError (String, BS.ByteString)
validateDeckImagePayload request = do
  let providedMime = normalizeMime (mimeType request)
  let rawPayload = filter (not . isSpace) (base64Data request)
  let (payloadMimeRaw, encodedData) = case parseDataUrl rawPayload of
        Just pair -> pair
        Nothing -> (providedMime, rawPayload)
  let payloadMime = normalizeMime payloadMimeRaw
  let finalMime = if providedMime == "" then payloadMime else providedMime
  extension <- maybe (Left unsupportedMimeType) Right (mimeToExtension finalMime)
  decoded <- either (const $ Left invalidImage) Right (B64.decode $ BS8.pack encodedData)
  if BS.length decoded <= maxDeckImageBytes
    then Right (extension, decoded)
    else Left imageTooLarge
  where
    invalidImage = Unauthorized "Invalid deck image payload."
    unsupportedMimeType = Unauthorized "Unsupported image type. Allowed: image/jpeg, image/png, image/webp."
    imageTooLarge = Unauthorized "Deck image is too large. Max 5MB."

normalizeMime :: String -> String
normalizeMime = map toLower . filter (not . isSpace)

mimeToExtension :: String -> Maybe String
mimeToExtension "image/jpeg" = Just "jpg"
mimeToExtension "image/jpg" = Just "jpg"
mimeToExtension "image/png" = Just "png"
mimeToExtension "image/webp" = Just "webp"
mimeToExtension _ = Nothing

parseDataUrl :: String -> Maybe (String, String)
parseDataUrl raw = do
  afterPrefix <- stripPrefix "data:" raw
  let (header, rest) = break (== ',') afterPrefix
  encoded <- stripPrefix "," rest
  mime <- stripSuffix ";base64" header
  pure (mime, encoded)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str =
  reverse <$> stripPrefix (reverse suffix) (reverse str)
