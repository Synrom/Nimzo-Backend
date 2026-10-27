{-# LANGUAGE OverloadedStrings #-}
module App.OnboardingLimits (newOnboardingLimits) where

import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (status413, status429)
import Network.Socket (SockAddr(..))
import Network.Wai

-- A bounded per-peer, per-route fixed-window limiter. Forwarded-IP headers are
-- deliberately not trusted. Behind a proxy, configure equivalent public limits
-- there and set a suitable aggregate API limit before production rollout.
newOnboardingLimits :: IO Middleware
newOnboardingLimits = do
  state <- newIORef Map.empty
  pure $ \app request respond -> case policy (rawPathInfo request) of
    Nothing -> app request respond
    Just requestLimit -> do
      now <- (`div` 1000000000) <$> getMonotonicTimeNSec
      let key = (peer (remoteHost request),rawPathInfo request)
      allowed <- atomicModifyIORef' state $ \entries ->
        let live = Map.filter (\(started,_) -> now-started<60) entries
        in case Map.lookup key live of
          Just (started,count) | count<requestLimit -> (Map.insert key (started,count+1) live,True)
          Just _ -> (live,False)
          Nothing | Map.size live<4096 -> (Map.insert key (now,1::Int) live,True)
          Nothing -> (live,False)
      if not allowed
        then respond $ responseLBS status429 [("Content-Type","application/json"),("Retry-After","60")] "{\"msg\":\"Onboarding request limit reached.\"}"
        else do
          bounded <- collect request 0 []
          case bounded of
            Nothing -> respond $ responseLBS status413 [("Content-Type","application/json")] "{\"msg\":\"Onboarding request body exceeds 256 KiB.\"}"
            Just chunks -> do
              remaining <- newIORef chunks
              let readCached = atomicModifyIORef' remaining $ \xs -> case xs of
                    [] -> ([],BS.empty)
                    x:rest -> (rest,x)
              app request {requestBody=readCached} respond
  where
    policy "/onboarding/revenuecat" = Just 600
    policy _ = Nothing
    peer (SockAddrInet _ address) = show address
    peer (SockAddrInet6 _ _ address _) = show address
    peer (SockAddrUnix path) = path
    collect request size chunks = do
      chunk <- getRequestBodyChunk request
      let nextSize = size+BS.length chunk
      if nextSize>262144 then pure Nothing
      else if BS.null chunk then pure $ Just (reverse chunks)
      else collect request nextSize (chunk:chunks)
