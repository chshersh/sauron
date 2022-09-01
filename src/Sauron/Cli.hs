{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Command-line arguments parsing
-}

module Sauron.Cli
    ( Cmd (..)
    , TopArgs (..)
    , cmdP
    ) where

import Options.Applicative as Opt


data Cmd
    = Top TopArgs

data TopArgs = TopArgs
    { topArgsUsername :: Text  -- ^ Twitter username handle
    , topArgsMax      :: Int   -- ^ Max top tweets to extract
    }

-- | All possible commands.
cmdP :: Opt.Parser Cmd
cmdP = Opt.subparser $ mconcat
    [ Opt.command "top"
          $ Opt.info (Opt.helper <*> topP)
          $ Opt.progDesc "Top tweets of a user"
    ]

topP :: Opt.Parser Cmd
topP = do
    topArgsUsername <- Opt.strArgument $ mconcat
        [ Opt.metavar "TWITTER_NAME"
        , Opt.help "Twitter user handle: @my-name or my-name"
        ]

    topArgsMax <- Opt.option Opt.auto $ mconcat
        [ Opt.long "max"
        , Opt.short 'm'
        , Opt.metavar "POSITIVE_NUMBER"
        , Opt.help "Max number of tweets to output in the terminal"
        ]

    pure $ Top TopArgs{..}
