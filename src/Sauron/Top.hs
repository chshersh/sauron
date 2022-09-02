{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'sauron top' command.

-}

module Sauron.Top
    ( runTop
    ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (minimum)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Servant.Client.Core (ClientError)

import Sauron.App (App)
import Sauron.Cli (CacheMode (..), TopArgs (..))
import Sauron.Top.Client (getTweets, getUserByUsername)
import Sauron.Top.Json (Data (..), Meta (..), Page (..))
import Sauron.Top.Tweet (Tweet (..), showTime, subtractSecond, topTweets)
import Sauron.Top.User (User (..), UserId (..), mkUsername)

import qualified Data.Aeson as Aeson


runTop :: TopArgs -> App ()
runTop TopArgs{..} = do
    let username = mkUsername topArgsUsername
    User{..} <- getUserByUsername username

    putTextLn $ "[info] User id of " <> topArgsUsername <> " is: " <> unUserId userId
    putTextLn $ "[info] Total number of tweets: " <> show userTweetCount

    tweets <- case topArgsCacheMode of
        ToFile toFile     -> timelineLoop userId toFile
        FromFile fromFile -> parseCachedTweets fromFile

    putTextLn $ topTweets topArgsMax username tweets

-- | Parse already saved tweets in the file
parseCachedTweets :: FilePath -> App [Tweet]
parseCachedTweets path =
    liftIO (Aeson.eitherDecodeFileStrict @[Tweet] path) >>= \case
        Right tweets -> pure tweets
        Left err -> do
            putStrLn $ "Error parsing " <> path <> ": " <> err
            exitFailure

{- | A data type representing the current state of fetching the
Twitter timeline
-}
data TimelineState
    -- | Fetching the timeline for the first time
    = Start

    -- | An error occurred; dump the current tweets to the file and exit with error
    | Error TimelineError [Tweet]

    -- | We haven't exhausted our queries
    | NextPage
        UTCTime -- ^ Timestamp used to fetch the previous page(s)
        Text    -- ^ Pagination token
        [Tweet]

    -- | We've reached the 3200 limit. Now change the end date.
    | NextDate
        UTCTime  -- ^ Time of the last tweet minus one second
        [Tweet]

    -- | Successfully fetched the entire timeline!
    | Finish [Tweet]

data TimelineError
    = InvalidRequest ClientError

displayTimelineError :: TimelineError -> Text
displayTimelineError = \case
    InvalidRequest err -> "[InvalidRequest] " <> show err

timelineLoop :: UserId -> FilePath -> App [Tweet]
timelineLoop userId savePath = loop Start
  where
    loop :: TimelineState -> App [Tweet]
    loop Start = do
        now <- liftIO getCurrentTime
        let endTime = showTime now
        getTweets userId endTime Nothing >>= \case
            Left err -> loop $ Error (InvalidRequest err) []
            Right Page{..} -> do
                let tweets = unData pageData
                case metaNextToken pageMeta of
                    Nothing ->
                        loop $ Finish $ unData pageData
                    Just nextToken ->
                        loop $ NextPage now nextToken tweets

    loop (Error err tweets) = do
        putTextLn $ "[error] " <> displayTimelineError err
        saveTweets savePath tweets
        exitFailure

    loop (Finish tweets) = do
        saveTweets savePath tweets
        pure tweets

    loop (NextPage time currentToken tweets) = do
        debugTweets tweets

        let endTime = showTime time
        getTweets userId endTime (Just currentToken) >>= \case
            Left err -> loop $ Error (InvalidRequest err) tweets
            Right Page{..} -> do
                let newTweets = unData pageData
                case metaNextToken pageMeta of
                    -- We fetched all pages
                    Nothing -> do
                        let earliestTime = minimum $ map tweetCreatedAt tweets
                        let newEndTime = subtractSecond earliestTime
                        loop $ NextDate newEndTime (newTweets ++ tweets)

                    -- More pages to fetch
                    Just nextToken ->
                        loop $ NextPage time nextToken (newTweets ++ tweets)

    loop (NextDate time tweets) = do
        debugTweets tweets

        let endTime = showTime time
        getTweets userId endTime Nothing >>= \case
            Left err -> loop $ Error (InvalidRequest err) tweets
            Right Page{..} -> do
                let newTweets = unData pageData
                case metaNextToken pageMeta of
                    -- We fetched all the tweets
                    Nothing ->
                        loop $ Finish (newTweets ++ tweets)

                    -- More pages to fetch
                    Just nextToken ->
                        loop $ NextPage time nextToken (newTweets ++ tweets)


debugTweets :: [Tweet] -> App ()
debugTweets tweets = do
    let len = length tweets
    putTextLn $ "[debug] Fetched total tweets: " <> show len

saveTweets :: FilePath -> [Tweet] -> App ()
saveTweets savePath tweets = do
    let tweetsCount = length tweets
    putTextLn $ "[info] Saving " <> show tweetsCount <> " tweets to: " <> toText savePath

    let json = encodePretty tweets
    writeFileLBS savePath json
