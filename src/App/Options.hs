{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module App.Options where

import Servant
import Servant.Foreign
import Servant.Auth (Auth)
import Network.Wai
import Data.Text hiding (null, zipWith, length)
import Network.HTTP.Types.Method
import Data.Maybe
import Data.List (nub)
import Network.HTTP.Types
import qualified Data.ByteString as B

instance (HasForeign lang ftype api) 
  => HasForeign lang ftype (Auth authSchemes user :> api) where
  type Foreign ftype (Auth authSchemes user :> api) = Foreign ftype api
  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

-- lang = NoTypes
-- ftype = NoContent
-- (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
provideOptions :: (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
               => Proxy api -> Middleware
provideOptions apiproxy app req cb
  | rmeth == "OPTIONS" = optional cb prior pinfo mlist
  | otherwise          = prior
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) apiproxy
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