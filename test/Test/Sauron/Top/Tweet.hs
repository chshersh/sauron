module Test.Sauron.Top.Tweet (tweetSpec) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Sauron.Top.Json (Data (..), Meta (..), Page (..))
import Sauron.Top.Tweet (Tweet (..))

import qualified Data.Aeson as Aeson


tweetSpec :: Spec
tweetSpec = describe "Tweet" $ do
    it "parses the example JSON tweet timeline" $
        Aeson.eitherDecodeFileStrict "example.json" `shouldReturn` Right exampleTimeline

exampleTimeline :: Page [Tweet]
exampleTimeline = Page
    { pageData = Data exampleTweets
    , pageMeta = Just Meta
        { metaResultCount = 5
        , metaNextToken = Just "7140dibdnow9c7btw4232rrxi2153ga6h81clvyarutkk"
        }
    }

exampleTweets :: [Tweet]
exampleTweets =
    [ Tweet
        { tweetId = "1565283709275803650"
        , tweetText = "@klarkc In this particular case, laziness wasn't the problem. Something weirder was going on 🤔"
        , tweetLikeCount = 0
        }
    , Tweet
        { tweetId = "1565246658128056320"
        , tweetText = "If you haven't finished your tea, I invite you to read one of my previous journeys into the wonders of the SQLite type system 🗺️🫖🚂\n\nhttps://t.co/4RElElHu0Z"
        , tweetLikeCount = 1
        }
    , Tweet
        { tweetId = "1565246655103995905"
        , tweetText = "🔮 I still had some theories about the space leak. All the metrics show that it's not the Haskell process that leaks memory.\n\nI was even suspecting space leaks in SQLite itself as my setup was unusual. But I didn't have the time to verify my hypothesis so we'll never know..."
        , tweetLikeCount = 1
        }
    , Tweet
        { tweetId = "1565246652121767936"
        , tweetText = "Are you still here? Do you still want to know how I solved this space leak? Very simple:\n\n🏝️ I left my job\n\nNow, it's no longer my problem and somebody else will continue what I've started 👶"
        , tweetLikeCount = 6
        }
    , Tweet
        { tweetId = "1565246649185849344"
        , tweetText = "🗺️ Now, real digging started. I had to use old Unix tools as my grandpa did in the good old times.\n\nI sampled the output of the /proc/rss values to check the real memory usage of my process. I've even looked at the diff between mmaped memory regions.\n\nStill, no results 🙅"
        , tweetLikeCount = 1
        }
    ]
