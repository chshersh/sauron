{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

User-related types in the 'sauron top' command.
-}

module Sauron.Top.User
    ( -- * Core types
      User (..)
    , UserId (..)
    , Username (..)
    , mkUsername
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Servant.API (ToHttpApiData)

import qualified Data.Text as Text


-- | A Twitter User ID like: "2244994945"
newtype UserId = UserId
    { unUserId :: Text
    } deriving newtype (FromJSON, ToHttpApiData)

{- | Stores Twitter Username handle without \@.

Use 'mkUsername' for safe creation.
-}
newtype Username = Username
    { unUsername :: Text
    } deriving newtype (ToHttpApiData)

-- | Strips \@ from the username
mkUsername :: Text -> Username
mkUsername username
    = Username
    $ fromMaybe username
    $ Text.stripPrefix "@" username

data User = User
    { userId         :: UserId
    , userTweetCount :: Int
    }


{- | Parses 'UserId' from JSON (excluding the data part, it's handled
by the 'Data' type):

@
{
  "data": {
    "id": "2164623379",
    "username": "ChShersh",
    "name": "Dmitrii Kovanikov",
    "public_metrics": {
      "followers_count": 2977,
      "following_count": 489,
      "tweet_count": 6502,
      "listed_count": 59
    }
  }
}
@
-}
instance FromJSON User where
    parseJSON = withObject "User" $ \o -> do
        userId <- o .: "id"

        publicMetrics <- o .: "public_metrics"
        userTweetCount <- publicMetrics .: "tweet_count"

        pure User{..}
