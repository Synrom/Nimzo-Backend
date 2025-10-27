{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Options where

import Servant
import Servant.Foreign
import Network.Wai
import Data.Text hiding (null, zipWith, length)
import Network.HTTP.Types.Method
import Data.Maybe
import Data.List (nub)
import Network.HTTP.Types
import qualified Data.ByteString as B

provideOptions :: [Req NoContent] -> Middleware
provideOptions mlist app req cb
  | rmeth == "OPTIONS" = optional cb prior pinfo mlist
  | otherwise          = prior
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  prior = app req cb

optional :: (Response -> r) -> r -> [Text] -> [Req NoContent] -> r
optional cb prior ts rs
  | null methods = prior
  | otherwise    = cb (buildResponse methods)
  where
  methods = mapMaybe (getMethod ts) rs

getMethod :: [Text] -> Req NoContent -> Maybe Method
getMethod rs ps
  | sameLength && matchingSegments = Just (_reqMethod ps)
  | otherwise                      = Nothing
  where
  pttrn          = _path $ _reqUrl ps
  sameLength       = length rs == length pttrn
  matchingSegments = and $ zipWith matchSegment rs pttrn

matchSegment :: Text -> Segment NoContent -> Bool
matchSegment a (Segment (Static (PathSegment b)) ) | a /= b = False
matchSegment _ _                                            = True

buildResponse :: [Method] -> Response
buildResponse ms = responseBuilder s h mempty
  where
  s = Status 200 "OK"
  m = B.intercalate ", " ("OPTIONS" : nub ms)
  h = [ ("Allow", m) ]