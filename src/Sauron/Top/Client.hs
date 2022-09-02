{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Functions for querying Twitter API.

__Auth:__

@
Authorization: Bearer $TWITTER_TOKEN
@
-}

module Sauron.Top.Client
    ( -- * User
      getUserByUsername
    , getTweets

      -- * Tweets
    , GetTweets

      -- * Internals
    , twitterBaseUrl
    ) where

import Control.Exception (throwIO)
import Servant.API (Capture, Get, Header', JSON, QueryParam, Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https), client, mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)

import Sauron.App (App, Env (..))
import Sauron.Top.Json (Data (..), Page (..))
import Sauron.Top.Tweet (Tweet)
import Sauron.Top.User (User, UserId, Username (..))

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
type GetUserByUsername
    =  RequiredHeader "Authorization" Text
    :> "users"
    :> "by"
    :> "username"
    :> Capture "username" Username
    :> QueryParam "user.fields" Text
    :> Get '[JSON] (Data User)

getUserByUsernameClient :: Text -> Username -> Maybe Text -> ClientM (Data User)
getUserByUsernameClient = client $ Proxy @GetUserByUsername

getUserByUsername :: Username -> App User
getUserByUsername username = do
    manager <- Iris.asksAppEnv envManager
    let clientEnv = mkClientEnv manager twitterBaseUrl

    token <- Iris.asksAppEnv envToken
    let auth = "Bearer " <> token

    let request = getUserByUsernameClient auth username (Just "public_metrics")

    res <- liftIO $ runClientM request clientEnv
    case res of
      Right (Data userId) -> pure userId
      Left err -> do
          putStrLn $ "Error getting user ID: " ++ show err
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
    :> QueryParam "pagination_token" Text
    :> Get '[JSON] (Page [Tweet])

getTweetsClient
    :: Text
    -> UserId
    -> Maybe Int
    -> Maybe Text
    -> Maybe Text
    -> Maybe String
    -> Maybe Text
    -> ClientM (Page [Tweet])
getTweetsClient = client $ Proxy @GetTweets

getTweetsClientRequest :: Text -> UserId -> String -> Maybe Text -> ClientM (Page [Tweet])
getTweetsClientRequest authToken userId endTime paginationToken = getTweetsClient
    authToken
    userId
    (Just 100)  -- Return 100 requests (max allowed)
    (Just "retweets")  -- exclude retweets & replies
    (Just "created_at,public_metrics")  -- extra fields to return
    (Just endTime)
    paginationToken

getTweets :: UserId -> String -> Maybe Text -> App (Either ClientError (Page [Tweet]))
getTweets userId endTime paginationToken = do
    manager <- Iris.asksAppEnv envManager
    let clientEnv = mkClientEnv manager twitterBaseUrl

    token <- Iris.asksAppEnv envToken
    let auth = "Bearer " <> token

    let request = getTweetsClientRequest
            auth
            userId
            endTime
            paginationToken

    liftIO $ runClientM request clientEnv
