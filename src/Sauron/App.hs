{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Main application monad and its environment to run the app.
-}

module Sauron.App
    ( App (..)
    ) where

import Sauron.Cli (Cmd)

import qualified Iris


newtype App a = App
    { unApp :: Iris.CliApp Cmd () a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv Cmd ())
        )
