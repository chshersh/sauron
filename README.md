# sauron

[![GitHub CI](https://github.com/chshersh/sauron/workflows/CI/badge.svg)](https://github.com/chshersh/sauron/actions)
[![Hackage](https://img.shields.io/hackage/v/sauron.svg?logo=haskell)](https://hackage.haskell.org/package/sauron)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

👁 `sauron` is a CLI tool that fetches info from Twitter and analyses it.

> 🌈 `sauron` is a demo project implemented using [Iris][iris] — a Haskell CLI
> framework.

[iris]: https://github.com/chshersh/iris

## Features

Features currently supported by `sauron`:

* Get top tweets of a Twitter account (limited by only 3200 recent tweets)
* Save intermediate results to a file (to avoid hitting Twitter API limit too early)
* Read cached results from a file

## How to use?

1. [Generate your own Twitter token][token] and export it as the
   `$TWITTER_TOKEN` variable.

2. Clone the project.

    ```shell
    git clone git@github.com:chshersh/sauron.git
    cd sauron
    ```

3. Build and run the tool
    
    > ⚠️ Requires GHC 9.2

    ```shell
    cabal run sauron -- top @<twitter-handle> --max=20 --to-file=path/to/save/results.json
    ```

[token]: https://developer.twitter.com/en/docs/twitter-api/getting-started/getting-access-to-the-twitter-api