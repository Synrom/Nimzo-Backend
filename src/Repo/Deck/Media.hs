{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Deck.Media (saveDeckImage) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import App.Env (deckImageDir, deckImagePublicBase)
import App.Error (AppError (Unauthorized))
import qualified Models.Deck as DeckModel
import Models.DeckImage (DeckImageUploadRequest (..), DeckImageUploadResponse (..))
import Repo.Classes
import qualified Repo.Deck.Query as DeckQuery
import Repo.Deck.Validation (normalizePublicBase, validateDeckImagePayload)
import Repo.Utils (ensure)

saveDeckImage :: MonadDB m => String -> Integer -> DeckImageUploadRequest -> m DeckImageUploadResponse
saveDeckImage username deckId payload = do
  deck <- DeckQuery.find deckId
  ensure notAuthor (DeckModel.author deck == username)
  (extension, imageBytes) <- fromEither (validateDeckImagePayload payload)
  env <- askEnv
  let fileName = "deck-" ++ show deckId ++ "." ++ extension
  let uploadPath = deckImageDir env </> fileName
  liftIO $ createDirectoryIfMissing True (deckImageDir env)
  liftIO $ BS.writeFile uploadPath imageBytes
  version <- floor <$> liftIO getPOSIXTime
  let imageUrl = normalizePublicBase (deckImagePublicBase env) ++ "/" ++ fileName ++ "?v=" ++ show version
  _ <- execute
    "UPDATE decks SET image_url = ?, last_modified = CURRENT_TIMESTAMP WHERE id = ?"
    (imageUrl, deckId)
  pure $ DeckImageUploadResponse imageUrl
  where
    notAuthor :: AppError
    notAuthor = Unauthorized "Only the deck author can upload a deck image."

fromEither :: MonadError e m => Either e a -> m a
fromEither = either throwError pure
