{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Core types in the 'sauron top' command.
-}

module Sauron.Top.Model
    ( -- * Core types
      UserId (..)
    , Username (..)
    , mkUsername

      -- * JSON parsing helpers
    , Data (..)
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Relude.Extra.Type (typeName)

import qualified Data.Text as Text


{- | Newtype wrapper for parsing JSON data inside the "data"
field. For example:

@
{ "data": ...
@
-}
newtype Data a = Data
    { unData :: a
    }

instance (FromJSON a, Typeable a) => FromJSON (Data a) where
    parseJSON = withObject ("Data " <> type_) $ \o -> do
        data_ <- o .: "data"
        pure $ Data data_
      where
        type_ :: String
        type_ = toString $ typeName @a

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
