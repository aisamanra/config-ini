{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Ini.Config.Bidir
import Data.Text (Text, unpack)
import Lens.Micro.TH (makeLenses)

data Config = Config
  { _confUsername      :: Text
  , _confPort          :: Int
  , _confUseEncryption :: Bool
  , _confHostname      :: Text
  } deriving (Eq, Show)

makeLenses ''Config

sampleConfig :: Config
sampleConfig = Config
  { _confUsername      = "<user>"
  , _confPort          = 8080
  , _confUseEncryption = True
  , _confHostname      = "localhost"
  }

parseConfig :: IniParser Config ()
parseConfig = section "NETWORK" $ do
  confUsername .= fieldOf "user" text <?>
    [ "your username" ]
  confPort .= fieldOf "port" number <?>
    [ "the port in question" ]
  confUseEncryption .= flagDef "encryption" True <?>
    [ "whether to use encryption (defaults to true)" ]
  confHostname .=? field "hostname" <?>
    [ "hostname to connect to (optional)" ]

example :: Text
example = "[NETWORK]\n\
          \user = gdritter\n\
          \port = 8888\n"

main :: IO ()
main = do
  print (parseIniFile sampleConfig parseConfig example)
  putStrLn (unpack (emitIniFile sampleConfig parseConfig))
