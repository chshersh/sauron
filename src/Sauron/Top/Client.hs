{-# LANGUAGE DataKinds     #-}
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
    ''
@

-}

module Sauron.Top.Client
    ( -- * User
      getUserIdByUsername

      -- * Tweets
    , GetTweets

      -- * Internals
    , twitterBaseUrl
    ) where

import Control.Exception (throwIO)
import Servant.API (Capture, Get, Header', JSON, QueryParam, Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https), client, mkClientEnv, runClientM)

import Sauron.App (App, Env (..))
import Sauron.Top.Json (Data (..), Page (..))
import Sauron.Top.Tweet (Tweet)
import Sauron.Top.User (UserId, Username (..))

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
type GetUserIdByUsername
    =  RequiredHeader "Authorization" Text
    :> "users"
    :> "by"
    :> "username"
    :> Capture "username" Username
    :> Get '[JSON] (Data UserId)

getUserIdByUsernameClient :: Text -> Username -> ClientM (Data UserId)
getUserIdByUsernameClient = client $ Proxy @GetUserIdByUsername

getUserIdByUsername :: Username -> App UserId
getUserIdByUsername username = do
    manager <- Iris.asksAppEnv envManager
    let clientEnv = mkClientEnv manager twitterBaseUrl

    token <- Iris.asksAppEnv envToken
    let auth = "Bearer " <> token

    res <- liftIO $ runClientM (getUserIdByUsernameClient auth username) clientEnv
    case res of
      Right (Data userId) -> pure userId
      Left err -> do
          putStrLn $ "Error: " ++ show err
          liftIO $ throwIO err

{- | API type for the following URL:

@
https://api.twitter.com/2/users/2164623379/tweets?max_results=5&exclude=retweets&tweet.fields=created_at,public_metrics&end_time=2022-09-01T13%3A52%3A14Z
@
-}
type GetTweets
    =  RequiredHeader "Authorization" Text
    :> "users"
    :> Capture "id" UserId
    :> "tweets"
    :> QueryParam "max_results" Int
    :> QueryParam "exclude" Text
    :> QueryParam "tweet.fields" Text
    :> QueryParam "end_time" String
    :> Get '[JSON] (Page [Tweet])
