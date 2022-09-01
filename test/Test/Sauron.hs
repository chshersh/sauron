module Test.Sauron (sauronSpec) where

import Test.Hspec (Spec, describe)

import Test.Sauron.Top (topSpec)


sauronSpec :: Spec
sauronSpec = describe "Sauron" $ do
    topSpec
