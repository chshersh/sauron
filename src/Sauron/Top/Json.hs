{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

JSON parsing helpers and common utilities
-}

module Sauron.Top.Json
    ( Data (..)
    , Meta (..)
    , Page (..)
    ) where

import Data.Aeson (FromJSON (..), Result (..), Value (Object), fromJSON, withObject, (.:), (.:?))
import Relude.Extra.Type (typeName)


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

{- | Parses relevant parts of the "meta" object

@
  "meta": {
    "result_count": 5,
    "newest_id": "1565283709275803650",
    "oldest_id": "1565246649185849344",
    "next_token": "7140dibdnow9c7btw4232rrxi2153ga6h81clvyarutkk"
  }
@
-}
data Meta = Meta
    { metaResultCount :: Int
    , metaNextToken   :: Maybe Text
    }

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \o -> do
        metaResultCount <- o .:  "result_count"
        metaNextToken   <- o .:? "next_token"
        pure Meta{..}

{- | A result of a pagination request
-}
data Page a = Page
    { pageData :: Data a
    , pageMeta :: Maybe Meta
    }

instance (FromJSON a, Typeable a) => FromJSON (Page a) where
    parseJSON = withObject ("Page " <> type_) $ \o -> do
        pageData <- case fromJSON @(Data a) (Object o) of
            Error err -> fail err
            Success a -> pure a
        pageMeta <- o .:? "meta"
        pure Page{..}
      where
        type_ :: String
        type_ = toString $ typeName @a
