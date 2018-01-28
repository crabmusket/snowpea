{-# LANGUAGE DeriveGeneric, QuasiQuotes, RecordWildCards #-}

module Podcast where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Aeson as JOSN
import qualified Web.JWT as JWT

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

instance JOSN.FromJSON PodcastInfo
instance JOSN.FromJSON PodcastEpisode

type TokenFactory = Text -> Int -> JWT.JSON

renderFeed :: TokenFactory -> ByteString -> Text -> PodcastInfo -> String
renderFeed makeToken basePath podcastId (PodcastInfo{..}) = [qc|<?xml version="1.0" encoding="UTF-8"?>
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

    <atom10:link xmlns:atom10="http://www.w3.org/2005/Atom" rel="self" type="application/rss+xml" href="{basePath}/podcasts/{podcastId}/feed" />

    {items}
  </channel>
</rss>|]
  where
    items = unlines $ map (renderEpisode makeToken basePath podcastId) episodes

renderEpisode :: TokenFactory -> ByteString -> Text -> PodcastEpisode -> String
renderEpisode makeToken basePath podcastId (PodcastEpisode{..}) = [qc|
    <item>
      <title>Show {index}: {title}</title>
      <guid>{url}</guid>
      <description>{notes}</description>
      <pubDate>{published}</pubDate>
      <enclosure url="{cs url :: String}?t={cs token :: String}" length="{size}" type="audio/mpeg" />
      <itunes:duration>{duration}</itunes:duration>
    </item>|]
  where
    url = cs basePath <> "/podcasts/" <> cs podcastId <> "/episodes/" <> show index <> "/download"
    token = makeToken podcastId index
