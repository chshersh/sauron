{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

The 'sauron top' command.

Query:

curl -X GET -H "Authorization: Bearer $TWITTER_TOKEN" 'https://api.twitter.com/2/users/2164623379/tweets?max_results=5&exclude=retweets&tweet.fields=created_at,public_metrics&end_time=2022-09-01T13%3A52%3A14Z'
-}

module Sauron.Run.Top
    ( runTop
    ) where

import Sauron.App (App)
import Sauron.Cli (TopArgs (..))


runTop :: TopArgs -> App ()
runTop TopArgs{..} = pure ()
