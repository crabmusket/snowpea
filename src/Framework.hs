{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- Instead of using a framework like Scotty or Yesod I prefer to work on top of
-- raw Wai. For a larger server I'd end up developing an ad-hoc framework with
-- exception handling and so on, but for 2 routes it's hardly worth it. This
-- collection of functions takes care of some basic 'framework' functionality
-- that Wai doesn't provide.
module Framework where

import Control.Monad (join)
import Data.ByteString (ByteString)

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

getHeader :: Wai.Request -> HTTP.HeaderName -> Maybe ByteString
getHeader request name = lookup name (Wai.requestHeaders request)

getQueryParam :: Wai.Request -> ByteString -> Maybe ByteString
getQueryParam request name = join (lookup name (Wai.queryString request))

-- Instead of just appending the header, as some example code does, this will
-- replace a header if it's set twice on a response.
setHeader :: Wai.Response -> (HTTP.HeaderName, ByteString) -> Wai.Response
setHeader response (name, value) = Wai.mapResponseHeaders replaceOrAddHeader response
  where
    replaceOrAddHeader headers = filter (\header -> fst header /= name) headers ++ [(name, value)]

-- Because I deploy behind a proxy, it's necessary to not trust the raw request
-- host, but to also look at the proxy header.
getProxiedHost :: Wai.Request -> Maybe ByteString
getProxiedHost request = case (proxied, actual) of
  (Just h, _) -> Just h
  (_, Just h) -> Just h
  (_, _) -> Nothing
  where
    proxied = getHeader request "X-Forwarded-Host"
    actual = getHeader request "Host"

-- The proxy server always communicates over HTTP to we also have to check a
-- custom header for the scheme.
getProxiedScheme :: Wai.Request -> ByteString
getProxiedScheme request = case getHeader request "X-Forwarded-Proto" of
  Just proto -> proto
  Nothing -> if Wai.isSecure request then "https" else "http"
