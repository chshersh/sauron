module Main (main) where

import Sauron (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
