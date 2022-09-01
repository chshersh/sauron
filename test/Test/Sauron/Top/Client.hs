module Test.Sauron.Top.Client (clientSpec) where

import Servant.Client.Core.BaseUrl (showBaseUrl)
import Servant.Links (allLinks, linkURI)
import Test.Hspec (Spec, describe, it, shouldBe)

import Sauron.Top.Client (GetTweets, twitterBaseUrl)


clientSpec :: Spec
clientSpec = describe "Client" $ do
    it "the GetTweets URL is correct" $
        generatedUrl `shouldBe` expectedUrl

generatedUrl :: Text
generatedUrl = mconcat
    [ toText $ showBaseUrl twitterBaseUrl
    , "/"
    , path
    ]
  where
    path :: Text
    path = show
         $ linkURI
         $ allLinks
             (Proxy @GetTweets)
             "2164623379"
             (Just 5)
             (Just "retweets")
             (Just "created_at,public_metrics")
             (Just "2022-09-01T13:52:14Z")

expectedUrl :: Text
expectedUrl = "https://api.twitter.com/2/users/2164623379/tweets?max_results=5&exclude=retweets&tweet.fields=created_at%2Cpublic_metrics&end_time=2022-09-01T13%3A52%3A14Z"
