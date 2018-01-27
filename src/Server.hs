{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Server where

import Data.Monoid ((<>))
import Control.Monad (join)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.String.Conversions (ConvertibleStrings, cs)
import qualified Data.Default as Default
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Json
import qualified Network.HTTP.Types as Http

import Podcast
import Framework

makeApp :: IO Wai.Application
makeApp = do
  username <- lookupEnvUnsafe "USERNAME"
  password <- lookupEnvUnsafe "PASSWORD"
  storageDir <- lookupEnvWithDefault "." "STORAGE_DIR"
  secret <- fmap JWT.secret (lookupEnvUnsafe "SECRET")
  let
    application request respond = (getHandler request) request respond
    tokenFactory = makeTokenFactory secret
    getHandler request = case (Wai.requestMethod request, Wai.pathInfo request) of
      ("GET", []) -> showIndex
      ("GET", ["feeds", pod]) -> getPodcastFeed username password pod tokenFactory
      ("GET", ["feeds", pod, "episodes", ep]) -> getPodcastEpisode secret storageDir pod ep
      ("GET", _otherwise) -> handleNotFound
      _otherwise -> handleMethodNotAllowed
  return application

showIndex :: Wai.Application
showIndex _ respond = respond (Wai.responseLBS Http.status200 [] "nothing to see here")

getPodcastFeed :: ByteString -> ByteString -> Text -> TokenFactory -> Wai.Application
getPodcastFeed username password podcastId tokenFactory request respond =
  if not (isAuthenticated username password request)
    then respond errorNotAuthenticated
    else do
      maybeInfo <- getPodcastInfo podcastId
      case maybeInfo of
        Nothing -> respond errorBadRequest
        Just info -> do
          let
            scheme = getProxiedScheme request
            host = getProxiedHost request `withDefault` "localhost:3000"
            basePath = scheme <> "://" <> host
            feed = renderFeed tokenFactory basePath podcastId info
            response = makeResponse Http.status200 feed
              `setHeader` ("Content-Type", "application/atom+xml")
          respond response

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
          response = Wai.responseFile Http.status200 [] filePath Nothing
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
        correctPodClaim = Map.lookup "pod" claims == Just (Json.String podcastId)
        correctEpisodeClaim = Map.lookup "ep" claims == Just (Json.String episodeId)

handleNotFound :: Wai.Application
handleNotFound _ respond = respond (Wai.responseLBS Http.status404 [] "not found")

handleMethodNotAllowed :: Wai.Application
handleMethodNotAllowed _ respond = respond (Wai.responseLBS Http.status405 [] "method not allowed")

errorNotAuthenticated :: Wai.Response
errorNotAuthenticated = Wai.responseLBS (toEnum 401) [] "invalid authentication"

errorBadRequest :: Wai.Response
errorBadRequest = Wai.responseLBS (toEnum 400) [] "bad request"

isAuthenticated :: ByteString -> ByteString -> Wai.Request -> Bool
isAuthenticated username password request = case maybeAuth of
    Nothing -> False
    Just auth -> auth == desired
  where
    maybeAuth = getHeader request "Authorization"
    desired = "Basic " <> Base64.encode (username <> ":" <> password)

getHeader :: Wai.Request -> Http.HeaderName -> Maybe ByteString
getHeader request name = lookup name (Wai.requestHeaders request)

getQueryParam :: Wai.Request -> ByteString -> Maybe ByteString
getQueryParam request name = join (lookup name (Wai.queryString request))

setHeader :: Wai.Response -> (Http.HeaderName, ByteString) -> Wai.Response
setHeader response (name, value) = Wai.mapResponseHeaders replaceOrAddHeader response
  where
    replaceOrAddHeader headers = filter (\header -> fst header /= name) headers ++ [(name, value)]

makeResponse :: ConvertibleStrings a LBS.ByteString => Http.Status -> a -> Wai.Response
makeResponse status content = Wai.responseLBS status [] (cs content)

getPodcastInfo :: Text -> IO (Maybe PodcastInfo)
getPodcastInfo podcastId = do
  contents <- LBS.readFile (cs podcastId <> ".json")
  case Json.eitherDecode contents of
    Left _ -> return Nothing
    Right value -> return (Just value)

getProxiedHost :: Wai.Request -> Maybe ByteString
getProxiedHost request = case (proxied, actual) of
  (Just h, _) -> Just h
  (_, Just h) -> Just h
  (_, _) -> Nothing
  where
    proxied = getHeader request "X-Forwarded-Host"
    actual = getHeader request "Host"

getProxiedScheme :: Wai.Request -> ByteString
getProxiedScheme request = case getHeader request "X-Forwarded-Proto" of
  Just scheme -> scheme
  Nothing -> if Wai.isSecure request then "https" else "http"

makeTokenFactory :: JWT.Secret -> TokenFactory
makeTokenFactory secret pod index = JWT.encodeSigned JWT.HS256 secret $ Default.def {
    JWT.unregisteredClaims = Map.fromList
      [ ("pod", Json.String (cs pod))
      , ("ep", Json.String (cs $ show index))
      ]
  }
