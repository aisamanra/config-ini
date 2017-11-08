module Main where

import           Data.List
import           Data.Ini.Config.Raw
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.Exit

dir :: FilePath
dir = "test/prewritten/cases"

main :: IO ()
main = do
  files <- getDirectoryContents dir
  let inis = [ f | f <- files
                 , ".ini" `isSuffixOf` f
                 ]
  mapM_ runTest inis

type IniSeq = Seq (Text, Seq (Text, Text))

toMaps :: RawIni -> IniSeq
toMaps (RawIni m) = fmap sectionToPair m
  where sectionToPair (name, section) =
          (normalizedText name, fmap valueToPair (isVals section))
        valueToPair (name, value) =
          (normalizedText name, T.strip (vValue value))

runTest :: FilePath -> IO ()
runTest iniF = do
  let hsF = take (length iniF - 4) iniF ++ ".hs"
  ini <- T.readFile (dir ++ "/" ++ iniF)
  hs  <- readFile (dir ++ "/" ++ hsF)
  case parseRawIni ini of
    Left err -> do
      putStrLn ("Error parsing " ++ iniF)
      putStrLn err
      exitFailure
    Right x
      | toMaps x == read hs -> do
          putStrLn ("Passed: " ++ iniF)
      | otherwise -> do
          putStrLn ("Parses do not match for " ++ iniF)
          putStrLn ("Expected: " ++ hs)
          putStrLn ("Actual: " ++ show (toMaps x))
          exitFailure
