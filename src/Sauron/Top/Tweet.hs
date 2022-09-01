{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'Tweet' type in the JSON API response.
-}

module Sauron.Top.Tweet
    ( Tweet (..)
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))


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
  "text": "🔮 I still had some theories about the space leak. All the metrics show that it's not the Haskell process that leaks memory.\n\nI was even suspecting space leaks in SQLite itself as my setup was unusual. But I didn't have the time to verify my hypothesis so we'll never know...",
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
