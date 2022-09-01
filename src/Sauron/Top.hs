{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'sauron top' command.

-}

module Sauron.Top
    ( runTop
    ) where

import Text.Pretty.Simple (pPrint)

import Sauron.App (App)
import Sauron.Cli (CacheMode (..), TopArgs (..))
import Sauron.Top.Client (getUserIdByUsername)
import Sauron.Top.Json (Page)
import Sauron.Top.Tweet (Tweet)
import Sauron.Top.User (UserId (..), mkUsername)

import qualified Data.Aeson as Aeson


runTop :: TopArgs -> App ()
runTop TopArgs{..} = do
    let username = mkUsername topArgsUsername
    userId <- getUserIdByUsername username

    putTextLn $ "User id of '" <> topArgsUsername <> "' is: " <> unUserId userId

    case topArgsCacheMode of
        ToFile _toFile    -> putTextLn "Doing nothing..."
        FromFile fromFile ->
            liftIO (Aeson.eitherDecodeFileStrict @(Page [Tweet]) fromFile) >>= \case
                Left err       -> putStrLn $ "Error: " <> err
                Right timeline -> pPrint timeline
