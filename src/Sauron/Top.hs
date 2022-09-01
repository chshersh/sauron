{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'sauron top' command.

-}

module Sauron.Top
    ( runTop
    ) where

import Sauron.App (App)
import Sauron.Cli (TopArgs (..))
import Sauron.Top.Model (UserId (..), mkUsername)
import Sauron.Top.Client (getUserIdByUsername)


runTop :: TopArgs -> App ()
runTop TopArgs{..} = do
    let username = mkUsername topArgsUsername
    userId <- getUserIdByUsername username

    putTextLn $ "User id of '" <> topArgsUsername <> "' is: " <> unUserId userId
