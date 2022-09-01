module Test.Sauron.Top (topSpec) where

import Test.Hspec (Spec, describe)

import Test.Sauron.Top.Client (clientSpec)
import Test.Sauron.Top.Tweet (tweetSpec)


topSpec :: Spec
topSpec = describe "Top" $ do
    clientSpec
    tweetSpec
