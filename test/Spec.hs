module Main (main) where

import Sauron (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
