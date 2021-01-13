{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ini.Config
import Data.Text (Text)
import qualified Data.Text.IO as T

data Config = Config
  {cfNetwork :: NetworkConfig, cfLocal :: Maybe LocalConfig}
  deriving (Eq, Show)

data NetworkConfig = NetworkConfig
  {netHost :: String, netPort :: Int}
  deriving (Eq, Show)

data LocalConfig = LocalConfig
  {localUser :: Text}
  deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
  netCf <- section "NETWORK" $ do
    host <- fieldOf "host" string
    port <- fieldOf "port" number
    return NetworkConfig {netHost = host, netPort = port}
  locCf <-
    sectionMb "LOCAL" $
      LocalConfig `fmap` field "user"
  return Config {cfNetwork = netCf, cfLocal = locCf}

main :: IO ()
main = do
  rs <- T.getContents
  print (parseIniFile rs configParser)
