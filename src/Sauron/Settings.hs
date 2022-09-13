{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Settings for Iris.
-}

module Sauron.Settings
    ( mkSettings
    ) where

import Sauron.Cli (Cmd, cmdP)
import Sauron.Env (Env)

import qualified Iris
import qualified Paths_sauron as Autogen


-- | Sauron application settings
mkSettings :: Env -> Iris.CliEnvSettings Cmd Env
mkSettings env = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsCmdParser = cmdP

    , Iris.cliEnvSettingsAppEnv = env

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
