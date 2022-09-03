module Test.Sauron.Top.Tweet (tweetSpec) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..), secondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hedgehog (Gen, forAll, tripping)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Hspec.Hedgehog (hedgehog)

import Sauron.Top.Tweet (Tweet (..), parseTime, showTime, subtractSecond)

import qualified Data.Aeson as Aeson
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tweetSpec :: Spec
tweetSpec = describe "Tweet" $ do
    it "parses the time" $ do
        parseTime "2022-09-01T07:54:18.000Z" `shouldBe`
            Just (UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:18"))

    it "shows the time" $ do
        showTime (UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:18"))
            `shouldBe` "2022-09-01T07:54:18Z"

    it "subtracts one second" $ do
        subtractSecond (UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:18"))
            `shouldBe` (UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:17"))

    it "parses the example JSON tweet timeline" $
        Aeson.eitherDecodeFileStrict "example.json" `shouldReturn` Right exampleTweets

    it "decodeJSON . encodeJSON = id" $ hedgehog $ do
        tweet <- forAll genTweet
        tripping tweet Aeson.encode Aeson.eitherDecode

parseTod :: String -> DiffTime
parseTod
    = fromMaybe (error "parsing time")
    . parseTimeM True defaultTimeLocale "%H:%M:%S"

exampleTweets :: [Tweet]
exampleTweets =
    [ Tweet
        { tweetId = "1565283709275803650"
        , tweetText = "@klarkc In this particular case, laziness wasn't the problem. Something weirder was going on 🤔"
        , tweetCreatedAt = UTCTime (fromGregorian 2022 9 1) (parseTod "10:21:32")
        , tweetLikeCount = 0
        , tweetRetweetCount = 0
        , tweetReplyCount = 0
        , tweetQuoteCount = 0
        }
    , Tweet
        { tweetId = "1565246658128056320"
        , tweetText = "If you haven't finished your tea, I invite you to read one of my previous journeys into the wonders of the SQLite type system 🗺️🫖🚂\n\nhttps://t.co/4RElElHu0Z"
        , tweetCreatedAt = UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:18")
        , tweetLikeCount = 1
        , tweetRetweetCount = 0
        , tweetReplyCount = 2
        , tweetQuoteCount = 0
        }
    , Tweet
        { tweetId = "1565246655103995905"
        , tweetText = "🔮 I still had some theories about the space leak. All the metrics show that it's not the Haskell process that leaks memory.\n\nI was even suspecting space leaks in SQLite itself as my setup was unusual. But I didn't have the time to verify my hypothesis so we'll never know..."
        , tweetCreatedAt = UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:18")
        , tweetLikeCount = 1
        , tweetRetweetCount = 0
        , tweetReplyCount = 1
        , tweetQuoteCount = 0
        }
    , Tweet
        { tweetId = "1565246652121767936"
        , tweetText = "Are you still here? Do you still want to know how I solved this space leak? Very simple:\n\n🏝️ I left my job\n\nNow, it's no longer my problem and somebody else will continue what I've started 👶"
        , tweetCreatedAt = UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:17")
        , tweetLikeCount = 6
        , tweetRetweetCount = 0
        , tweetReplyCount = 1
        , tweetQuoteCount = 0
        }
    , Tweet
        { tweetId = "1565246649185849344"
        , tweetText = "🗺️ Now, real digging started. I had to use old Unix tools as my grandpa did in the good old times.\n\nI sampled the output of the /proc/rss values to check the real memory usage of my process. I've even looked at the diff between mmaped memory regions.\n\nStill, no results 🙅"
        , tweetCreatedAt = UTCTime (fromGregorian 2022 9 1) (parseTod "07:54:16")
        , tweetLikeCount = 1
        , tweetRetweetCount = 0
        , tweetReplyCount = 1
        , tweetQuoteCount = 0
        }
    ]

genTweet :: Gen Tweet
genTweet = Tweet
    <$> Gen.text (Range.linear 1 20) Gen.digit
    <*> Gen.text (Range.linear 1 280) Gen.unicode
    <*> genUTCTime
    <*> Gen.int (Range.linear 0 100_000)
    <*> Gen.int (Range.linear 0 100_000)
    <*> Gen.int (Range.linear 0 100_000)
    <*> Gen.int (Range.linear 0 100_000)

genUTCTime :: Gen UTCTime
genUTCTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2022)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86401)
    let diff = secondsToDiffTime secs
    pure $ UTCTime day diff
