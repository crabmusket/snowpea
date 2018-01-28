{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Server where

import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.String.Conversions (cs)

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as Log
import qualified Web.JWT as JWT

import Podcast (TokenFactory, PodcastInfo, renderFeed)
import Framework
import Utils

-- makeApp is an IO action so I can do specific setup rather than dependency-injecting
-- all its needs from the application main function. In future it might be best
-- to have a specific config type that is created in application main and passed
-- to makeApp.
makeApp :: IO Wai.Application
makeApp = do
  username <- lookupEnvUnsafe "USERNAME"
  password <- lookupEnvUnsafe "PASSWORD"
  environment <- lookupEnvWithDefault "local" "PASSWORD"
  storageDir <- lookupEnvWithDefault "." "STORAGE_DIR"
  secret <- fmap JWT.secret (lookupEnvUnsafe "SECRET")
  let
    application request respond = (getHandler request) request respond
    -- The router is designed to correctly order 404 and 405 errors. If the resource
    -- does not exist (no route match), then 404 is returned no matter the method.
    -- If the route matches but the method cannot be handled, then selectMethod
    -- can return a 405.
    -- This style doesn't optimise well, but performance is not a consideration.
    tokenFactory = makeTokenFactory secret
    getHandler request = case Wai.pathInfo request of
      [] -> selectMethod
        [("GET", getIndex)]
      ["podcasts", pod, "feed"] -> selectMethod
        [("GET", getPodcastFeed username password storageDir pod tokenFactory)]
      ["podcasts", pod, "episodes", ep, "download"] -> selectMethod
        [("GET", getPodcastEpisode secret storageDir pod ep)]
      _otherwise -> handleNotFound
      where
        -- request is captured in this utility function
        selectMethod methods = case lookup (Wai.requestMethod request) methods of
          Just handler -> handler
          Nothing -> handleMethodNotAllowed
    -- Default logger takes IP addresses from the socket only, but since I deploy
    -- behind a proxy this isn't useful. So in production we'll use the
    -- FromFallback source.
    makeLogger = if environment == ("production" :: String)
      then Log.mkRequestLogger def { Log.outputFormat = Log.Apache Log.FromFallback }
      else return Log.logStdoutDev
  logger <- makeLogger
  return (logger application)

makeTokenFactory :: JWT.Secret -> TokenFactory
makeTokenFactory secret pod index = JWT.encodeSigned JWT.HS256 secret $ def
  { JWT.unregisteredClaims = Map.fromList
      [ ("pod", JSON.String (cs pod))
      , ("ep", JSON.String (cs $ show index))
      ]
  }

getIndex :: Wai.Application
getIndex _ respond = respond (Wai.responseLBS HTTP.status200 [] "nothing to see here")

-- My two main handlers have very 'leaning' structures that increase in depth.
-- This is beecause no exceptions or other forms of monadic error handling are
-- used. I think it makes the logic quite clear. Some boilerplace could be reduced
-- by using exceptions and handling them at the application level.
getPodcastFeed :: ByteString -> ByteString -> FilePath -> Text -> TokenFactory -> Wai.Application
getPodcastFeed username password storageDir podcastId tokenFactory request respond =
  if not (isAuthenticated username password request)
    then respond errorNotAuthenticated
    else do
      maybeInfo <- loadPodcastInfo storageDir podcastId
      case maybeInfo of
        Nothing -> respond errorBadRequest
        Just info -> do
          let
            scheme = getProxiedScheme request
            host = getProxiedHost request `withDefault` "localhost:3000"
            basePath = scheme <> "://" <> host
            feedContent = renderFeed tokenFactory basePath podcastId info
            response = Wai.responseLBS HTTP.status200 [] (cs feedContent)
              `setHeader` ("Content-Type", "application/atom+xml")
          respond response

-- This doesn't extend very well to multiple accounts, since the username and
-- password are passed in explicitly. A more robust checker would take an IO
-- action checking the username/password combination found in the header.
isAuthenticated :: ByteString -> ByteString -> Wai.Request -> Bool
isAuthenticated username password request = case maybeAuth of
  Nothing -> False
  Just auth -> auth == desired
  where
    maybeAuth = getHeader request "Authorization"
    desired = "Basic " <> Base64.encode (username <> ":" <> password)

loadPodcastInfo :: FilePath -> Text -> IO (Maybe PodcastInfo)
loadPodcastInfo storageDir podcastId = do
  contents <- LBS.readFile (storageDir <> "/" <> cs podcastId <> ".json")
  case JSON.eitherDecode contents of
    Left _ -> return Nothing
    Right value -> return (Just value)

getPodcastEpisode :: JWT.Secret -> FilePath -> Text -> Text -> Wai.Application
getPodcastEpisode secret storageDir podcastId episodeId request respond = do
  let maybeToken = getQueryParam request "t"
  case maybeToken of
    Nothing -> respond errorBadRequest
    Just rawToken -> if not (verifyEpisodeToken secret podcastId episodeId (cs rawToken))
      then respond errorBadRequest
      else do
        let
          filePath = storageDir <> "/" <> cs podcastId <> "/" <> cs episodeId <> ".mp3"
          response = Wai.responseFile HTTP.status200 [] filePath Nothing
            `setHeader` ("Content-Type", "audio/mpeg")
        respond response

verifyEpisodeToken :: JWT.Secret -> Text -> Text -> JWT.JSON -> Bool
verifyEpisodeToken secret podcastId episodeId rawToken = case parsedToken of
  Nothing -> False
  Just token -> claimsAreValid (getClaims token)
  where
    parsedToken = JWT.decodeAndVerifySignature secret rawToken
    getClaims = JWT.unregisteredClaims . JWT.claims
    claimsAreValid claims = correctPodClaim && correctEpisodeClaim
      where
        correctPodClaim = Map.lookup "pod" claims == Just (JSON.String podcastId)
        correctEpisodeClaim = Map.lookup "ep" claims == Just (JSON.String episodeId)

handleNotFound :: Wai.Application
handleNotFound _ respond = respond (Wai.responseLBS HTTP.status404 [] "not found")

handleMethodNotAllowed :: Wai.Application
handleMethodNotAllowed _ respond = respond (Wai.responseLBS HTTP.status405 [] "method not allowed")

errorNotAuthenticated :: Wai.Response
errorNotAuthenticated = Wai.responseLBS (toEnum 401) [] "invalid authentication"

errorBadRequest :: Wai.Response
errorBadRequest = Wai.responseLBS (toEnum 400) [] "bad request"
