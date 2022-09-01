{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The eye that watches everything you did on Twitter
-}

module Sauron
    ( main
    ) where

import Sauron.App (App (..))
import Sauron.Cli (Cmd (..), cmdP)
import Sauron.Run (app)

import qualified Iris
import qualified Paths_sauron as Autogen


appSettings :: Iris.CliEnvSettings Cmd ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsCmdParser = cmdP

    , Iris.cliEnvSettingsHeaderDesc =
        "sauron - an evil eye that watches your Twitter"

    , Iris.cliEnvSettingsProgDesc =
        "A CLI tool to get insights from Twitter (top tweets, etc.)"

    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = \v -> "Sauron v" <> v
            }

    , Iris.cliEnvSettingsRequiredTools = []
    }

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
