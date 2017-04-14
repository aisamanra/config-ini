{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Foldable as Fold
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Ini as I1
import qualified Data.Ini.Config.Raw as I2
import           Data.List (nub)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_iniEquiv :: Property
prop_iniEquiv = property $ do
  raw <- forAll mkIni
  let printed = I1.printIniWith I1.defaultWriteIniSettings raw
      i1 = I1.parseIni printed
      i2 = I2.parseIni printed
  case (i1, i2) of
   (Right i1', Right i2') ->
     let i1'' = lower i1'
         i2'' = toMaps i2'
     in i1'' === i2''
   _ -> failure

prop_revIniEquiv :: Property
prop_revIniEquiv = property $ do
  raw <- forAll mkRichIni
  let printed = I2.printIni raw
      i1 = I1.parseIni printed
      i2 = I2.parseIni printed
  case (i1, i2) of
   (Right i1', Right i2') ->
     lower i1' === toMaps i2'
   _ -> failure

prop_iniSelfEquiv :: Property
prop_iniSelfEquiv = property $ do
  raw <- forAll mkRichIni
  Right (toMaps raw) === fmap toMaps (I2.parseIni (I2.printIni raw))

lower :: I1.Ini -> HashMap Text (HashMap Text Text)
lower (I1.Ini ini) = go (fmap go ini)
  where go hm = HM.fromList [ (T.toLower k, v) | (k, v) <- HM.toList hm ]

toMaps :: I2.Ini -> HashMap Text (HashMap Text Text)
toMaps (I2.Ini m) = conv (fmap sectionToPair m)
  where sectionToPair (name, section) =
          (name, conv (fmap valueToPair (I2.isVals section)))
        valueToPair (name, value) = (T.toLower name, T.strip (I2.vValue value))
        conv = HM.fromList . Fold.toList

textChunk :: Monad m => Gen.Gen m Text
textChunk = fmap T.pack $ Gen.list (Range.linear 1 20) $ Gen.alphaNum

mkIni :: Monad m => Gen.Gen m I1.Ini
mkIni = do
  ss <- Gen.list (Range.linear 0 10) $ do
    name <- textChunk
    section <- Gen.list (Range.linear 0 10) $
      (,) <$> textChunk <*> textChunk
    return (name, HM.fromList section)
  return (I1.Ini (HM.fromList ss))

mkComments :: Monad m => Gen.Gen m (Seq.Seq I2.BlankLine)
mkComments = fmap (Seq.fromList . nub) $ Gen.list (Range.linear 0 5) $
  Gen.choice
    [ return I2.BlankLine
    , I2.CommentLine <$> Gen.element ";#" <*> textChunk
    ]

mkRichIni :: Monad m => Gen.Gen m I2.Ini
mkRichIni = do
  ss <- Gen.list (Range.linear 0 10) $ do
    name <- textChunk
    section <- Gen.list (Range.linear 0 10) $ do
      k <- textChunk
      v <- textChunk
      cs <- mkComments
      return ( T.toLower k
             , I2.IniValue 0 k v cs False '='
             )
    cs <- mkComments
    return ( T.toLower name
           , I2.IniSection name (Seq.fromList section) 0 0 cs
           )
  return (I2.Ini (Seq.fromList ss))

main :: IO ()
main = do
  _ <- $$(checkConcurrent)
  return ()
