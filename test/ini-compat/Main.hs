module Main where

import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Ini as I1
import qualified Data.Ini.Config.Raw as I2
import           Data.Text (Text)
import qualified Data.Text as T

import           Test.QuickCheck

iniEquiv :: I1.Ini -> Bool
iniEquiv raw = case (i1, i2) of
   (Right i1', Right i2') ->
     let i1'' = lower i1'
         i2'' = toMaps i2'
     in i1'' == i2''
   _ -> False
  where pr = I1.printIniWith I1.defaultWriteIniSettings raw
        i2 = I2.parseIni pr
        i1 = I1.parseIni pr

lower :: I1.Ini -> HashMap Text (HashMap Text Text)
lower (I1.Ini hm) =
  HM.fromList [ (T.toLower k, v) | (k, v) <- HM.toList hm ]

toMaps :: I2.Ini -> HashMap Text (HashMap Text Text)
toMaps (I2.Ini m) = fmap (fmap I2.vValue . I2.isVals) m

instance Arbitrary I1.Ini where
  arbitrary = (I1.Ini . HM.fromList) <$> listOf sections
    where sections = (,) <$> str <*> section
          str = (T.pack <$> arbitrary) `suchThat` (\ t ->
                   T.all (\ c -> isAlphaNum c || c == ' ')
                   t && not (T.null t))
          section = HM.fromList <$> listOf kv
          kv = (,) <$> str <*> str

main :: IO ()
main = quickCheck iniEquiv
