{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Main application monad and its environment to run the app.
-}

module Sauron.App
    ( App (..)
    , Env (..)
    , runApp
    ) where

import Control.Exception (throwIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Error (userError)

import Sauron.Cli (Cmd, cmdP)

import qualified Iris
import qualified Paths_sauron as Autogen


-- | Sauron CLI application monad.
newtype App a = App
    { unApp :: Iris.CliApp Cmd Env a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv Cmd Env)
        )

-- | Sauron CLI application environment.
data Env = Env
    { envManager :: Manager
    , envToken   :: Text
    }

-- | Sauron application settings
appSettings :: Env -> Iris.CliEnvSettings Cmd Env
appSettings env = Iris.defaultCliEnvSettings
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

-- | Create application environment.
mkEnv :: IO Env
mkEnv = do
    envManager <- newTlsManager

    envToken <- lookupEnv "TWITTER_TOKEN" >>= \case
        Just token -> pure $ toText token
        Nothing -> throwIO $ userError "The environment variable 'TWITTER_TOKEN' not found"

    pure Env{..}

{- | Run the Sauron CLI application monad by:

  1. Creating application environment.
  2. Creating application settings.
  3. Running the monadic action.
-}
runApp :: App a -> IO a
runApp app = do
    env <- mkEnv
    let settings = appSettings env
    Iris.runCliApp settings $ unApp app
