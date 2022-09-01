{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'Tweet' type in the JSON API response.
-}

module Sauron.Top.Tweet
    ( Tweet (..)
    , topTweets
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))

import Sauron.Top.User (Username (..))

import qualified Data.Text as Text


data Tweet = Tweet
    { tweetId        :: Text
    , tweetText      :: Text
    , tweetLikeCount :: Int
    } deriving stock (Show, Eq)

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

        publicMetrics <- o .: "public_metrics"
        tweetLikeCount <- publicMetrics .: "like_count"

        pure Tweet{..}

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
        [ "URL:   " <> tweetUrl
        , "Likes: " <> show tweetLikeCount
        , "Text:"
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
