{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The eye that watches everything you did on Twitter
-}

module Sauron
    ( main
    ) where

import Sauron.App (App, runApp)
import Sauron.Cli (Cmd (..))
import Sauron.Top (runTop)

import qualified Iris


main :: IO ()
main = runApp app

app :: App ()
app = Iris.asksCliEnv Iris.cliEnvCmd >>= \case
    Top topArgs -> runTop topArgs
