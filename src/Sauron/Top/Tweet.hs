{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'Tweet' type in the JSON API response.
-}

module Sauron.Top.Tweet
    ( Tweet (..)
    , topTweets

      -- * Internals
    , parseTime
    , showTime
    , subtractSecond
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (formatParseM, iso8601Format)

import Sauron.Top.User (Username (..))

import qualified Data.Text as Text


data Tweet = Tweet
    { tweetId           :: Text
    , tweetText         :: Text
    , tweetCreatedAt    :: UTCTime
    , tweetLikeCount    :: Int
    , tweetRetweetCount :: Int
    , tweetReplyCount   :: Int
    , tweetQuoteCount   :: Int
    } deriving stock (Show, Eq)

parseTime :: String -> Maybe UTCTime
parseTime = formatParseM iso8601Format

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

subtractSecond :: UTCTime -> UTCTime
subtractSecond = addUTCTime (-1)

{- | Parses Tweet from the following JSON object:

@
{
  "public_metrics": {
    "retweet_count": 0,
    "reply_count": 1,
    "like_count": 1,
    "quote_count": 0
  },
  "text": "🔮 I still had some theories about the space leak...",
  "created_at": "2022-09-01T07:54:18.000Z",
  "id": "1565246655103995905"
}
@
-}
instance FromJSON Tweet where
    parseJSON = withObject "Tweet" $ \o -> do
        tweetId   <- o .: "id"
        tweetText <- o .: "text"

        createdAt <- o .: "created_at"
        tweetCreatedAt <- case parseTime createdAt of
            Nothing   -> fail $ "Error parsing time: " <> createdAt
            Just time -> pure time

        publicMetrics <- o .: "public_metrics"
        tweetLikeCount    <- publicMetrics .: "like_count"
        tweetRetweetCount <- publicMetrics .:? "retweet_count" .!= 0
        tweetReplyCount   <- publicMetrics .:? "reply_count"   .!= 0
        tweetQuoteCount   <- publicMetrics .:? "quote_count"   .!= 0

        pure Tweet{..}

instance ToJSON Tweet where
    toJSON Tweet{..} = object
        [ "id"         .= tweetId
        , "created_at" .= showTime tweetCreatedAt
        , "text"       .= tweetText
        , "public_metrics" .= object
            [ "like_count"    .= tweetLikeCount
            , "retweet_count" .= tweetRetweetCount
            , "reply_count"   .= tweetReplyCount
            , "quote_count"   .= tweetQuoteCount
            ]
        ]

-- | Extract top N tweets and pretty format them.
topTweets :: Int -> Username -> [Tweet] -> Text
topTweets maxTweets username
    = formatTweets username
    . take maxTweets
    . sortWith (Down . tweetLikeCount)

formatTweets :: Username -> [Tweet] -> Text
formatTweets _ [] = "No tweets found"
formatTweets username tweets = foldMap formatTweet tweets
  where
    formatTweet :: Tweet -> Text
    formatTweet Tweet{..} = unlines
        [ "URL          : " <> tweetUrl
        , "Tweeted at   : " <> toText (showTime tweetCreatedAt)
        , "Likes        : " <> show tweetLikeCount
        , "Awesome text :"
        , chunkedText
        ]
      where
        tweetUrl :: Text
        tweetUrl = mconcat
            [ "https://twitter.com/"
            , unUsername username
            , "/status/"
            , tweetId
            ]

        chunkedText :: Text
        chunkedText
            = unlines
            $ map ("    " <>)
            $ Text.chunksOf 60 tweetText
