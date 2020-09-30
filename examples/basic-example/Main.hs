{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ini.Config
import Data.Text (Text)

data Config = Config
  { confUsername :: Text,
    confPort :: Int,
    confUseEncryption :: Bool
  }
  deriving (Eq, Show)

parseConfig :: IniParser Config
parseConfig = section "network" $ do
  user <- field "user"
  port <- fieldOf "port" number
  enc <- fieldFlagDef "encryption" True
  return (Config user port enc)

example :: Text
example =
  "[NETWORK]\n\
  \user = gdritter\n\
  \port = 8888\n"

main :: IO ()
main = do
  print (parseIniFile example parseConfig)
