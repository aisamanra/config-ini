{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Ini.Config.Lens
import Data.Text (Text)
import Lens.Micro.Platform (makeLenses)

data Config = Config
  { _confUsername      :: Text
  , _confPort          :: Int
  , _confUseEncryption :: Bool
  } deriving (Eq, Show)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
  { _confUsername      = "undefined"
  , _confPort          = 0
  , _confUseEncryption = True
  }

parseConfig :: IniLensParser Config ()
parseConfig = sectionL "network" $ do
  confUsername      .=  field     "user"
  confPort          .=  fieldOf   "port" number
  confUseEncryption .=? fieldMbOf "encryption" flag

example :: Text
example = "[NETWORK]\n\
          \user = gdritter\n\
          \port = 8888\n"

main :: IO ()
main = do
  print (parseIniFileL example defaultConfig parseConfig)
