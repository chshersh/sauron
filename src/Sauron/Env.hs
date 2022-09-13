{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Sauron application monad environment.
-}

module Sauron.Env
    ( Env (..)
    , mkEnv
    ) where

import Control.Exception (throwIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.IO.Error (userError)


-- | A type storing all app environment fields.
data Env = Env
    { envManager :: Manager
    , envToken   :: Text
    }

-- | Create application environment.
mkEnv :: IO Env
mkEnv = do
    envManager <- newTlsManager

    envToken <- lookupEnv "TWITTER_TOKEN" >>= \case
        Just token -> pure $ toText token
        Nothing    -> throwIO $ userError "The environment variable 'TWITTER_TOKEN' not found"

    pure Env{..}
