{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Functions for contacting Twitter API:

__Auth:__

@
Authorization: Bearer $TWITTER_TOKEN
@

__Getting tweets by user ID:__

@
curl \
    -H "Authorization: Bearer $TWITTER_TOKEN" \
    'https://api.twitter.com/2/users/2164623379/tweets?max_results=5&exclude=retweets&tweet.fields=created_at,public_metrics&end_time=2022-09-01T13%3A52%3A14Z'
@

-}

module Sauron.Top.Client
    ( getUserIdByUsername
    ) where

import Control.Exception (throwIO)
import Servant.API (Capture, Get, JSON, (:>), Required, Strict, Header')
import Servant.Client (BaseUrl (..), Scheme (Https), ClientM, client, runClientM, mkClientEnv)

import Sauron.App (App, Env (..))
import Sauron.Top.Model (Data (..), UserId, Username (..))

import qualified Iris


-- | URL for acessing Twitter API v2
twitterBaseUrl :: BaseUrl
twitterBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost   = "api.twitter.com"
    , baseUrlPort   = 443
    , baseUrlPath   = "/2"
    }

type RequiredHeader = Header' '[Required, Strict]

{- | API type for the following URL:

@
https://api.twitter.com/2/users/by/username/:username
@
-}
type UserIdByUsername
    =  RequiredHeader "Authorization" Text
    :> "users"
    :> "by"
    :> "username"
    :> Capture "username" Text
    :> Get '[JSON] (Data UserId)

userIdByUsername :: Text -> Text -> ClientM (Data UserId)
userIdByUsername = client $ Proxy @UserIdByUsername

getUserIdByUsername :: Username -> App UserId
getUserIdByUsername (Username username) = do
  manager <- Iris.asksAppEnv envManager
  let clientEnv = mkClientEnv manager twitterBaseUrl

  token <- Iris.asksAppEnv envToken
  let auth = "Bearer " <> token

  res <- liftIO $ runClientM (userIdByUsername auth username) clientEnv
  case res of
    Right (Data userId) -> pure userId
    Left err -> do
        putStrLn $ "Error: " ++ show err
        liftIO $ throwIO err
