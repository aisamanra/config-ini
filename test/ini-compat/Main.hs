module Main where

import           Data.Char
import qualified Data.Foldable as Fold
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Ini as I1
import qualified Data.Ini.Config.Raw as I2
import           Data.List (nub)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T

import           Test.QuickCheck

myArgs :: Args
myArgs = stdArgs

iniEquiv :: ArbIni -> Bool
iniEquiv (ArbIni raw) = case (i1, i2) of
   (Right i1', Right i2') ->
     let i1'' = lower i1'
         i2'' = toMaps i2'
     in i1'' == i2''
   _ -> False
  where pr = I1.printIniWith I1.defaultWriteIniSettings raw
        i2 = I2.parseIni pr
        i1 = I1.parseIni pr

revIniEquiv :: RichIni -> Bool
revIniEquiv (RichIni raw) = case (i1, i2) of
   (Right i1', Right i2') ->
     let i1'' = lower i1'
         i2'' = toMaps i2'
     in i1'' == i2''
   _ -> False
   where pr = I2.printIni raw
         i1 = I1.parseIni pr
         i2 = I2.parseIni pr

lower :: I1.Ini -> HashMap Text (HashMap Text Text)
lower (I1.Ini hm) =
  HM.fromList [ (T.toLower k, v) | (k, v) <- HM.toList hm ]

toMaps :: I2.Ini -> HashMap Text (HashMap Text Text)
toMaps (I2.Ini m) = conv (fmap sectionToPair m)
  where sectionToPair (name, section) = ( name
                                        , conv (fmap valueToPair (I2.isVals section))
                                        )
        valueToPair (name, value) = (name, I2.vValue value)
        conv = HM.fromList . Fold.toList

newtype ArbText = ArbText { fromArbText :: Text } deriving (Show)

instance Arbitrary ArbText where
  arbitrary = (ArbText . T.pack) `fmap` listOf1 (arbitrary `suchThat` isValid)
    where isValid ':' = False
          isValid '=' = False
          isValid '#' = False
          isValid ';' = False
          isValid '[' = False
          isValid ']' = False
          isValid c
            | isSpace c = False
            | otherwise = True

newtype ArbIni = ArbIni I1.Ini deriving (Show)

instance Arbitrary ArbIni where
  arbitrary = (ArbIni . I1.Ini . HM.fromList) `fmap` listOf sections
    where sections = do
            name <- str
            sec  <- section
            return (name, sec)
          str = fromArbText `fmap` arbitrary
          section = HM.fromList `fmap` listOf kv
          kv = do
            name <- str
            val  <- str
            return (name, val)

newtype RichIni = RichIni (I2.Ini) deriving (Show)

instance Arbitrary RichIni where
  arbitrary = (RichIni . I2.Ini . Seq.fromList . nub) `fmap` listOf sections
    where sections = do
            name <- (T.toLower . T.strip) `fmap` str
            sec  <- section name
            return (name, sec)
          str = fromArbText `fmap` arbitrary
          section n = do
            vals <- listOf kv
            cs <- Seq.fromList `fmap` listOf comment
            return (I2.IniSection n (Seq.fromList $ nub vals) 0 0 cs)
          kv = do
            name <- T.strip `fmap` str
            val  <- str
            cs   <- Seq.fromList `fmap` listOf comment
            return (name, I2.IniValue 0 name val cs False '=')
          comment = oneof [ return I2.BlankLine
                          , do { c <- elements ";#"
                               ; txt <- str
                               ; return (I2.CommentLine c txt)
                               }
                          ]

main :: IO ()
main = do
  quickCheckWith myArgs revIniEquiv
  quickCheckWith myArgs iniEquiv
