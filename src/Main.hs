{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards, DeriveGeneric #-}

import Web.Scotty
import System.Environment (lookupEnv)
import Data.Maybe (maybe)
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Control.Monad (join)
import Data.Text (unpack)
import Data.Text.Lazy (pack)
import Data.Text.Encoding (decodeUtf8)
import Text.InterpolatedString.Perl6 (qc)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Default as Default
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Json

main :: IO ()
main = do
  username <- readEnvUnsafe "USERNAME"
  password <- readEnvUnsafe "PASSWORD"
  secret <- fmap JWT.secret (readEnvUnsafe "SECRET")
  port <- readEnvWithDefault "PORT" 3000

  scotty port $ do
    get "/" $ do
      text "nothing to see here"

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

readEnvUnsafe variable = do
  maybeVal <- lookupEnv variable
  case maybeVal of
    Nothing -> error $ "please provide environment variable " <> variable
    Just value -> return (cs value)

readEnvWithDefault :: Read a => String -> a -> IO a
readEnvWithDefault variable def = do
  maybeVal <- lookupEnv variable
  let parsed = join $ fmap readMaybe maybeVal
  return $ maybe def id parsed

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
