{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

This module runs the 'sauron' executable.
-}

module Sauron.Run
    ( app
    ) where

import Sauron.App (App)
import Sauron.Cli (Cmd (..))
import Sauron.Run.Top (runTop)

import qualified Iris


app :: App ()
app = Iris.asksCliEnv Iris.cliEnvCmd >>= \case
    Top topArgs -> runTop topArgs
