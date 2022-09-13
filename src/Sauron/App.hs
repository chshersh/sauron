{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Main application monad and its environment to run the app.
-}

module Sauron.App
    ( App (..)
    , runApp
    ) where

import Sauron.Cli (Cmd)
import Sauron.Env (Env, mkEnv)
import Sauron.Settings (mkSettings)

import qualified Iris

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

{- | Run the Sauron CLI application monad by:

  1. Creating application environment.
  2. Constructing application settings.
  3. Running the monadic action.
-}
runApp :: App a -> IO a
runApp app = do
    env <- mkEnv
    let settings = mkSettings env
    Iris.runCliApp settings $ unApp app
