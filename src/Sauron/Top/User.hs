{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

User-related types in the 'sauron top' command.
-}

module Sauron.Top.User
    ( -- * Core types
      UserId (..)
    , Username (..)
    , mkUsername
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))

import qualified Data.Text as Text


-- | A Twitter User ID like: "2244994945"
newtype UserId = UserId
    { unUserId :: Text
    }

{- |

Parses 'UserId' from JSON (excluding the data part, it's handled by the 'Data' type):

@
{ "data": {
      "id":"2164623379",
      "name":"Dmitrii Kovanikov",
      "username":"ChShersh"
  }
}
@
-}
instance FromJSON UserId where
    parseJSON = withObject "UserId" $ \o -> do
        uid <- o .: "id"
        pure $ UserId uid

{- | Stores Twitter Username handle without \@.

Use 'mkUsername' for safe creation.
-}
newtype Username = Username
    { unUsername :: Text
    }

-- | Strips \@ from the username
mkUsername :: Text -> Username
mkUsername username
    = Username
    $ fromMaybe username
    $ Text.stripPrefix "@" username

-- {- | Object representing all the user tweets in their timeline.
-- data Timeline
