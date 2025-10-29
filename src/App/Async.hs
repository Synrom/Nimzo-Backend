module App.Async where

import App.AppM
import App.Env
import App.Error
import Servant (runHandler)
import Servant.Server (errReasonPhrase)
import Control.Concurrent.Async (async)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

appMToIO :: Env -> AppM a -> IO (Either AppError a)
appMToIO env action = do
  handlerResult <- runHandler $ runExceptT $ runReaderT (runAppM action) env
  case handlerResult of
    Left servantErr -> 
      return $ Left $ Internal $ errReasonPhrase servantErr
    Right result -> return result

voidWithLogs :: IO (Either AppError a) -> IO ()
voidWithLogs io = do
  result <- io
  case result of
    Left err -> print err
    Right _ -> pure ()

forkAppM :: AppM () -> AppM ()
forkAppM action = do
  env <- ask
  liftIO $ void $ async $ voidWithLogs $ appMToIO env action