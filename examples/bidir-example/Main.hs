{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
  , _confConfigFile    :: Maybe Text
  , _confPath          :: [Text]
  } deriving (Eq, Show)

makeLenses ''Config

sampleConfig :: Config
sampleConfig = Config
  { _confUsername      = "<user>"
  , _confPort          = 8080
  , _confUseEncryption = True
  , _confHostname      = "localhost"
  , _confConfigFile    = Nothing
  , _confPath          = ["/bin"]
  }

configSpec :: Ini Config
configSpec = ini sampleConfig $ do
  section "NETWORK" $ do
    confUsername .= field "user" text
      & comment [ "your username" ]
    confPort .= field "port" number
      & comment [ "the port in question" ]
    confUseEncryption .= flag "encryption"
      & skipIfMissing
      & comment [ "whether to use encryption (defaults to true)" ]
    confHostname .= field "hostname" text
      & skipIfMissing
      & comment [ "hostname to connect to (optional)" ]
    confConfigFile .=? field "config file" text
      & placeholderValue "<file path>"
  section "LOCAL" $ do
    confPath .= field "path" (listWithSeparator ":" text)
      & skipIfMissing
      & comment [ "a colon-separated path list" ]

example :: Text
example = "[NETWORK]\n\
          \# this contains a comment\n\
          \; and a semicolon comment\n\
          \user: gdritter\n\
          \port: 8888\n"

main :: IO ()
main = do
  let s = parseIni example configSpec
  case s of
    Left err -> putStrLn err
    Right p  -> do
      let v = getIniValue p
      print v
      putStrLn "------------------------"
      putStr (unpack (getIniText configSpec))
      putStrLn "------------------------"
      let v' = v { _confPort = 9191
                 , _confHostname = "argl"
                 , _confPath = "/usr/sbin" : _confPath v
                 }
      let pol = defaultUpdatePolicy
                  { updateGeneratedCommentPolicy =
                      CommentPolicyAddDefaultComment
                        [ "value added by application" ]
                  , updateIgnoreExtraneousFields = False
                  }
      let up = getIniText $ updateIni v' $ setIniUpdatePolicy pol p
      putStrLn "------------------------"
      putStr (unpack up)
      putStrLn "------------------------"
