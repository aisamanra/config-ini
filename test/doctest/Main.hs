module Main where

import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest [ "src/Data/Ini/Config.hs", "-XOverloadedStrings"  ]
  doctest [ "src/Data/Ini/Config/Lens.hs", "-XRankNTypes", "-XOverloadedStrings" ]
