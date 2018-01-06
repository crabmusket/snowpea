
{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards, DeriveGeneric #-}
import System.Environment (lookupEnv)
import Data.Monoid ((<>))
import Control.Monad (join)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Text.InterpolatedString.Perl6 (qc)
import Data.String.Conversions (ConvertibleStrings, cs)
import GHC.Generics (Generic)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as Text
import qualified Text.Read as Text
--import qualified Data.Text.Encoding as Text
import qualified Data.Default as Default
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Json
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Status as Http

main :: IO ()
main = do
  username <- readEnvUnsafe "USERNAME" :: IO ByteString
  password <- readEnvUnsafe "PASSWORD" :: IO ByteString
  --secret <- fmap JWT.secret (readEnvUnsafe "SECRET")
  port <- readEnvWithDefault "PORT" 3000
  let
    app :: Wai.Application
    app request respond = case Wai.pathInfo request of
      [] -> showIndex request respond
      ["feeds", pod] -> getPodcastFeed username password pod request respond
      ["feeds", pod, "eps", ep] -> getPodcastEpisode pod ep request respond
      _otherwise -> handleNotFound request respond
  putStrLn ("running on port " <> show port)
  Warp.run port app

showIndex :: Wai.Application
showIndex _ respond = respond (Wai.responseLBS Http.status200 [] "nothing to see here")

getPodcastFeed :: ByteString -> ByteString -> Text -> Wai.Application
getPodcastFeed username password podcastId request respond = do
  authenticated <- isAuthenticated username password request
  maybeInfo <- getPodcastInfo podcastId
  case (authenticated, maybeInfo) of
    (False, _) -> respond errorNotAuthenticated
    (_, Nothing) -> respond errorBadRequest
    (True, Just info) -> do
      let host = Wai.requestHeaderHost request `withDefault` "localhost:3000"
      let scheme = if Wai.isSecure request then "https://" else "http://"
      respond $ response Http.status200 (scheme <> host <> " " <> cs podcastId)
      {-
      setHeader "content-type" "application/atom+xml"
      text $ pack $ renderFeed (tokenMaker secret) podcastId (cs $ scheme <> host) info
      -}

getPodcastEpisode :: Text -> Text -> Wai.Application
getPodcastEpisode podcastId episodeId request respond = respond (Wai.responseLBS Http.status200 [] "nothing to see here")

handleNotFound :: Wai.Application
handleNotFound _ respond = respond (Wai.responseLBS Http.status404 [] "not found")

errorNotAuthenticated :: Wai.Response
errorNotAuthenticated = Wai.responseLBS (toEnum 401) [] "invalid authentication"

errorBadRequest :: Wai.Response
errorBadRequest = Wai.responseLBS (toEnum 400) [] "bad request"

isAuthenticated :: ByteString -> ByteString -> Wai.Request -> IO Bool
isAuthenticated username password request = do
  let
    maybeAuth = header request "Authorization"
    desired = "Basic " <> Base64.encode (username <> ":" <> password)
  case maybeAuth of
    Nothing -> return False
    Just auth -> return (cs auth == desired)

header :: Wai.Request -> Http.HeaderName -> Maybe ByteString
header request name = lookup name (Wai.requestHeaders request)

readEnvUnsafe :: IsString a => String -> IO a
readEnvUnsafe variable = do
  maybeVal <- lookupEnv variable
  case maybeVal of
    Nothing -> error $ "please provide environment variable " <> variable
    Just value -> return (fromString value)

readEnvWithDefault :: Read a => String -> a -> IO a
readEnvWithDefault variable defaultValue = do
  maybeValue <- lookupEnv variable
  let parsed = join (fmap Text.readMaybe maybeValue)
  return (parsed `withDefault` defaultValue)

withDefault :: Maybe a -> a -> a
maybeVal `withDefault` def = case maybeVal of
  Nothing -> def
  Just val -> val

response :: ConvertibleStrings a LBS.ByteString => Http.Status -> a -> Wai.Response
response status content = Wai.responseLBS status [] (cs content)

data PodcastInfo
  = PodcastInfo
    { name :: String
    , description :: String
    , summary :: String
    , image :: String
    , link :: String
    , feed_published :: String
    , keywords :: String
    , email :: String
    , author :: String
    , episodes :: [PodcastEpisode]
    } deriving (Show, Generic)

data PodcastEpisode
  = PodcastEpisode
    { index :: Int
    , title :: String
    , notes :: String
    , published :: String
    , duration :: String
    , size :: Int
    } deriving (Show, Generic)

instance Json.FromJSON PodcastInfo
instance Json.FromJSON PodcastEpisode

getPodcastInfo :: Text -> IO (Maybe PodcastInfo)
getPodcastInfo podcastId = do
  contents <- LBS.readFile (cs podcastId <> ".json")
  case Json.eitherDecode contents of
    Left err -> return Nothing
    Right value -> return (Just value)

{-
    get "/feeds/:pod" $ do
      pod <- param "pod"
      authenticated <- checkBasicAuth username password
      maybeInfo <- getPodcastInfo pod
      case (authenticated, maybeInfo) of
        (False, _) -> errorNotAuthenticated
        (_, Nothing) -> errorBadRequest
        (True, Just info) -> do
          host <- getRequestHost
          scheme <- getRequestScheme
          setHeader "content-type" "application/atom+xml"
          text $ pack $ renderFeed (tokenMaker secret) pod (cs $ scheme <> host) info

    get "/eps/:pod/:episode" $ do
      pod <- param "pod"
      episode <- param "episode"
      rawToken <- param "t"
      if not $ verifyEpisodeToken secret rawToken pod episode
        then errorBadRequest
        else do
            let path = pod <> "/" <> episode <> ".mp3"
            setHeader "content-type" "audio/mpeg"
            file (cs path)

getRequestHost = do
  host <- header "host"
  proxy <- header "x-forwarded-host"
  return $ case (proxy, host) of
    (Just host, _) -> host
    (_, Just host) -> host
    (_, _) -> ""

getRequestScheme = do
  forwardedScheme <- header "x-forwarded-proto"
  secure <- fmap Wai.isSecure request
  return $ case forwardedScheme of
    Just scheme -> scheme <> "://"
    Nothing -> (if secure then "https" else "http") <> "://"

errorNotAuthenticated :: ActionM ()
errorNotAuthenticated = do
  status (toEnum 401)
  text "invalid authentication"

errorBadRequest :: ActionM ()
errorBadRequest = do
  status (toEnum 400)
  text "bad request"

checkBasicAuth username password = do
  maybeAuth <- header "Authorization"
  return $ case maybeAuth of
    Nothing -> False
    Just header -> cs header == encoded
  where
    encoded = "Basic " <> Base64.encode (username <> ":" <> password)

getPodcastInfo pod = do
  contents <- liftAndCatchIO $ LBS.readFile (pod <> ".json")
  let result = Json.eitherDecode contents
  case result of
    Left err -> {- liftAndCatchIO (print err) >> -} return Nothing
    Right value -> return (Just value)

data PodcastInfo
  = PodcastInfo
    { name :: String
    , description :: String
    , summary :: String
    , image :: String
    , link :: String
    , feed_published :: String
    , keywords :: String
    , email :: String
    , author :: String
    , episodes :: [PodcastEpisode]
    } deriving (Show, Generic)

data PodcastEpisode
  = PodcastEpisode
    { index :: Int
    , title :: String
    , notes :: String
    , published :: String
    , duration :: String
    , size :: Int
    } deriving (Show, Generic)

instance Json.FromJSON PodcastInfo
instance Json.FromJSON PodcastEpisode

verifyEpisodeToken secret rawToken pod episode =
  let
    maybeToken = JWT.decodeAndVerifySignature secret rawToken
    getClaims = JWT.unregisteredClaims . JWT.claims
    correctPodClaim claims = Map.lookup "pod" claims == Just (Json.String pod)
    correctEpisodeClaim claims = Map.lookup "ep" claims == Just (Json.String episode)
    claimsAreValid claims = correctPodClaim claims && correctEpisodeClaim claims
  in case maybeToken of
    Nothing -> False
    Just token -> claimsAreValid (getClaims token)

renderFeed makeToken pod host (PodcastInfo{..}) = [qc|<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet type="text/xsl" media="screen" href="/~d/styles/rss2enclosuresfull.xsl"?>
<?xml-stylesheet type="text/css" media="screen" href="http://feeds.feedburner.com/~d/styles/itemcontent.css"?>
<rss xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" xmlns:media="http://search.yahoo.com/mrss/" xmlns:atom="http://www.w3.org/2005/Atom" version="2.0">
  <channel>
    <title>{name}</title>
    <itunes:summary>{summary}</itunes:summary>
    <itunes:subtitle>{description}</itunes:subtitle>
    <description>{description}</description>
    <language>en-us</language>
    <link>{link}</link>
    <itunes:image href="{image}" />
    <pubDate>{feed_published}</pubDate>
    <itunes:explicit>no</itunes:explicit>
    <managingEditor>{email} ({author})</managingEditor>
    <webMaster>{email} ({author})</webMaster>
    <itunes:author>{author}</itunes:author>
    <copyright>{author}</copyright>

    <image>
        <url>{image}</url>
        <link>{image}</link>
        <title>{name}</title>
    </image>

    <itunes:owner>
      <itunes:name>{author}</itunes:name>
      <itunes:email>{email}</itunes:email>
    </itunes:owner>

    <itunes:keywords>{keywords}</itunes:keywords>

    <atom10:link xmlns:atom10="http://www.w3.org/2005/Atom" rel="self" type="application/rss+xml" href="$url_prefix/podcast.atom" />
    <feedburner:info xmlns:feedburner="http://rssnamespace.org/feedburner/ext/1.0" uri="dancarlin/history" />
    <atom10:link xmlns:atom10="http://www.w3.org/2005/Atom" rel="hub" href="http://pubsubhubbub.appspot.com/" />
    {items}
  </channel>
</rss>|]
  where
    items = unlines $ map (renderEpisode makeToken pod host) episodes

renderEpisode makeToken pod host (PodcastEpisode{..}) = [qc|
    <item>
      <title>Show {index}: {title}</title>
      <guid>{cs url :: String}</guid>
      <description>{notes}</description>
      <link>http://www.dancarlin.com</link>
      <pubDate>{published}</pubDate>
      <enclosure url="{cs url :: String}?t={cs token :: String}" length="{size}" type="audio/mpeg" />
      <itunes:duration>{duration}</itunes:duration>
    </item>|]
  where
    url = host <> "/eps/" <> pod <> "/" <> show index
    token = makeToken pod index

tokenMaker :: JWT.Secret -> String -> Int -> JWT.JSON
tokenMaker secret pod index = JWT.encodeSigned JWT.HS256 secret $ Default.def {
    JWT.unregisteredClaims = Map.fromList
      [ ("pod", Json.String (cs pod))
      , ("ep", Json.String (cs $ show index))
      ]
  }
-}
