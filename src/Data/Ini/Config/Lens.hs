{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ini.Config.Lens
(
-- $main
-- * Running Lens-Based Parsers
  parseIniFileL
-- * Lens-Aware Parser Types
, IniLensParser
, SectionLensParser
-- * Lens-Aware Section-Level Parsing
, sectionL
, sectionOptL
-- * Lens-Aware Field-Level Parsing
, lensField
, (.=)
, lensFieldOpt
, (.=?)
-- ** Lens-Aware Field Parsing Aliases
, fieldL
, fieldOfL
, fieldMbL
, fieldMbOfL
, fieldOptL
, fieldOptOfL
, fieldDefL
, fieldDefOfL
, fieldFlagL
, fieldFlagDefL
-- * Reader Functions
, Lens
, updateLens
, module Data.Ini.Config
) where

import           Control.Applicative (Applicative(..), Alternative(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.Strict
import           Data.Ini.Config
import           Data.Monoid (Endo(..))
import           Data.Text (Text)

-- $setup
-- >>> type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
--
-- >>> let lens get set f a = (`set` a) `fmap` f (get a)
--
-- >>> let _1 = lens fst (\ a (_, b) -> (a, b))
--
-- >>> let _2 = lens snd (\ b (a, _) -> (a, b))

-- | This is a "lens"-compatible type alias
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- We need this to implement 'set' for lenses
newtype I a = I { fromI :: a }
instance Functor I where fmap f (I a) = I (f a)

set :: Lens s s a a -> a -> s -> s
set lens x a = fromI (lens (\ _ -> I x) a)

-- | This is a function compatible with the @fieldOf@ family of functions. It allows
--   you to parse a field and then create an update function with it.
updateLens :: (Text -> Either String a) -> Lens s s a a -> Text -> Either String (s -> s)
updateLens rd lens text = do
  case rd text of
    Left err -> Left err
    Right r  -> Right (\ st -> set lens r st)

newtype IniLensParser s a = IniLensParser (WriterT (Endo s) IniParser a)
  deriving (Functor, Applicative, Alternative, Monad)

newtype SectionLensParser s a = SectionLensParser (WriterT (Endo s) SectionParser a)
  deriving (Functor, Applicative, Alternative, Monad)

parseIniFileL :: Text -> s -> IniLensParser s () -> Either String s
parseIniFileL text def (IniLensParser mote) = do
  (_, Endo update) <- parseIniFile text (runWriterT mote)
  return (update def)

sectionL :: Text -> SectionLensParser s () -> IniLensParser s ()
sectionL name (SectionLensParser thunk) = IniLensParser $ do
  ((), update) <- lift (section name (runWriterT thunk))
  tell update
  return ()

sectionOptL :: Text -> SectionLensParser s () -> IniLensParser s ()
sectionOptL name (SectionLensParser thunk) = IniLensParser $ do
  updateMb <- lift (sectionMb name (runWriterT thunk))
  case updateMb of
    Nothing           -> return ()
    Just ((), update) -> tell update

toLens :: Lens s s a a -> SectionParser a -> SectionLensParser s ()
toLens lens mote = SectionLensParser $ do
  rs <- lift mote
  tell $ Endo (set lens rs)

-- | The 'lensField' function (or its operator form '.=') turns a lens and a
--   standard 'SectionParser' field into a 'SectionLensParser' that uses the
--   relevant lens to update an internal value to the result of the
--   'SectionParser'.
--
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (lensField _1 (field "x"))
-- Right ("hello",False)
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (lensField _1 (field "y"))
-- Left "Missing field \"y\" in section \"MAIN\""
lensField :: Lens s s a a -> SectionParser a -> SectionLensParser s ()
lensField = toLens

-- | An infix alias for 'lensField'.
--
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (_1 .= field "x")
-- Right ("hello",False)
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (_1 .= field "y")
-- Left "Missing field \"y\" in section \"MAIN\""
(.=) :: Lens s s a a -> SectionParser a -> SectionLensParser s ()
(.=) = toLens

-- | The 'lensFieldOpt' function (or its operator form '.=?') turns a lens
--   and a standard 'SectionParser' field into a 'SectionLensParser' that
--   ignores values that are not present, but uses the lens to set a value
--   that is present.
--
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (lensFieldOpt _1 (fieldMb "x"))
-- Right ("hello",False)
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (lensFieldOpt _1 (fieldMb "y"))
-- Right ("def",False)
lensFieldOpt :: Lens s s a a -> SectionParser (Maybe a) -> SectionLensParser s ()
lensFieldOpt lens mote = SectionLensParser $ do
  rsMb <- lift mote
  case rsMb of
    Just rs -> tell $ Endo (set lens rs)
    Nothing -> return ()

-- | An infix alias for 'lensFieldOpt'.
--
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (_1 .=? fieldMb "x")
-- Right ("hello",False)
-- >>> parseIniFileL"[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (_1 .=? fieldMb "y")
-- Right ("def",False)
(.=?) :: Lens s s a a -> SectionParser (Maybe a) -> SectionLensParser s ()
(.=?) = lensFieldOpt

-- | A 'Lens'-aware variant of 'field': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldL "x" _1)
-- Right ("hello",False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldL "y" _1)
-- Left "Missing field \"y\" in section \"MAIN\""
fieldL :: Text -> Lens s s Text Text -> SectionLensParser s ()
fieldL name lens = toLens lens $ field name

-- | A 'Lens'-aware variant of 'fieldOf': the 'Lens' argument names the
--   setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldOfL "x" number _1)
-- Right (72,False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (0, False) $ sectionL "MAIN" (fieldOfL "x" number _1)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldOfL "y" number _1)
-- Left "Missing field \"y\" in section \"MAIN\""
fieldOfL :: Text -> (Text -> Either String a) -> Lens s s a a -> SectionLensParser s ()
fieldOfL name rd lens = toLens lens $ fieldOf name rd


-- | A 'Lens'-aware variant of 'fieldMb': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (Just "def", False) $ sectionL "MAIN" (fieldMbL "x" _1)
-- Right (Just "hello",False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (Just "def", False) $ sectionL "MAIN" (fieldMbL "y" _1)
-- Right (Nothing,False)
fieldMbL :: Text -> Lens s s (Maybe Text) (Maybe Text) -> SectionLensParser s ()
fieldMbL name lens = toLens lens $ fieldMb name


-- | A 'Lens'-aware variant of 'fieldMbOf': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (Just 0, False) $ sectionL "MAIN" (fieldMbOfL "x" number _1)
-- Right (Just 72,False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (Just 0, False) $ sectionL "MAIN" (fieldMbOfL "x" number _1)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (Just 0, False) $ sectionL "MAIN" (fieldMbOfL "y" number _1)
-- Right (Nothing,False)
fieldMbOfL :: Text -> (Text -> Either String a) -> Lens s s (Maybe a) (Maybe a) -> SectionLensParser s ()
fieldMbOfL name rd lens = toLens lens $ fieldMbOf name rd

-- | A 'Lens'-aware variant of 'field' which does nothing if a key
--   is absent. The 'Lens' argument names the setter to use on the
--   underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldOptL "x" _1)
-- Right ("hello",False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldOptL "y" _1)
-- Right ("def",False)
fieldOptL :: Text -> Lens s s Text Text -> SectionLensParser s ()
fieldOptL name lens = SectionLensParser $ do
  rsMb <- lift (fieldMb name)
  case rsMb of
    Nothing -> return ()
    Just rs -> tell $ Endo (set lens rs)

-- | A 'Lens'-aware variant of 'fieldOf', which does nothing if a key
--  is absent. The 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldOptOfL "x" number _1)
-- Right (72,False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (0, False) $ sectionL "MAIN" (fieldOptOfL "x" number _1)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldOptOfL "y" number _1)
-- Right (0,False)
fieldOptOfL :: Text -> (Text -> Either String a) -> Lens s s a a -> SectionLensParser s ()
fieldOptOfL name rd lens = SectionLensParser $ do
  rsMb <- lift (fieldMbOf name rd)
  case rsMb of
    Nothing -> return ()
    Just rs -> tell $ Endo (set lens rs)

-- | A 'Lens'-aware variant of 'fieldDef': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("orig", False) $ sectionL "MAIN" (fieldDefL "x" "def" _1)
-- Right ("hello",False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("orig", False) $ sectionL "MAIN" (fieldDefL "y" "def" _1)
-- Right ("def",False)
fieldDefL :: Text -> Text -> Lens s s Text Text -> SectionLensParser s ()
fieldDefL name def lens = toLens lens $ fieldDef name def

-- | A 'Lens'-aware variant of 'fieldDefOf': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldDefOfL "x" number 99 _1)
-- Right (72,False)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" (0, False) $ sectionL "MAIN" (fieldDefOfL "x" number 99 _1)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileL "[MAIN]\nx = 72\n" (0, False) $ sectionL "MAIN" (fieldDefOfL "y" number 99 _1)
-- Right (99,False)
fieldDefOfL :: Text -> (Text -> Either String a) -> a -> Lens s s a a -> SectionLensParser s ()
fieldDefOfL name rd def lens = toLens lens $ fieldDefOf name rd def

-- | A 'Lens'-aware variant of 'fieldFlag': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = yes\n" ("def", False) $ sectionL "MAIN" (fieldFlagL "x" _2)
-- Right ("def",True)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldFlagL "x" _2)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a boolean"
-- >>> parseIniFileL "[MAIN]\nx = yes\n" ("def", False) $ sectionL "MAIN" (fieldFlagL "y" _2)
-- Left "Missing field \"y\" in section \"MAIN\""
fieldFlagL :: Text -> Lens s s Bool Bool -> SectionLensParser s ()
fieldFlagL name lens = toLens lens $ fieldFlag name

-- | A 'Lens'-aware variant of 'fieldFlagDef': the 'Lens' argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileL "[MAIN]\nx = yes\n" ("def", False) $ sectionL "MAIN" (fieldFlagDefL "x" False _2)
-- Right ("def",True)
-- >>> parseIniFileL "[MAIN]\nx = hello\n" ("def", False) $ sectionL "MAIN" (fieldFlagDefL "x" False _2)
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a boolean"
-- >>> parseIniFileL "[MAIN]\nx = yes\n" ("def", False) $ sectionL "MAIN" (fieldFlagDefL "y" False _2)
-- Right ("def",False)
fieldFlagDefL :: Text -> Bool -> Lens s s Bool Bool -> SectionLensParser s ()
fieldFlagDefL name def lens = toLens lens $ fieldFlagDef name def


-- $main
-- This module is designed to be used with lenses, so that we can
-- start with a default configuration and gradually update it,
-- rather than construct a new value from scratch. Among other
-- things, this makes it nicer to section our API but keep all
-- the configuration together. Consider the same example code
-- that appears in the documentation for the "Data.Ini.Config"
-- module, that parses this kind of configuration:
--
-- > [NETWORK]
-- > host = example.com
-- > port = 7878
-- >
-- > # here is a comment
-- > [LOCAL]
-- > user = terry
--
-- In that example, we split the configuration into a @NetworkConfig@
-- and a @LocalConfig@ type to mirror the configuration file's use of
-- @[LOCAL]@ and @[NETWORK]@ sections, but we might want to keep the
-- configuration data type as a single flag record, in which case our
-- parsing code becomes more awkward:
--
-- > data Config = Config
-- >   { _cfHost :: String
-- >   , _cfPort :: Int
-- >   , _cfUser :: Text
-- >   } deriving (Eq, Show)
-- >
-- > -- this is not ideal
-- > configParser :: IniParser Config
-- > configParser = do
-- >   (host, port) <- section "NETWORK" $ do
-- >     host <- fieldOf "host" string
-- >     port <- fieldOf "port" number
-- >     return (host, port)
-- >   user <- section "LOCAL" $ field "user"
-- >   return (Config host port user)
--
-- We could also use repeated invocations of 'section', but this
-- also makes our parsing code a lot uglier and involves unnecessary
-- repetition of the @\"NETWORK\"@ literal:
--
-- > -- this is kind of ugly
-- > configParser :: IniParser Config
-- > configParser = do
-- >   host <- section "NETWORK" $ fieldOf "host" string
-- >   port <- section "NETWORK" $ fieldOf "port" number
-- >   user <- section "LOCAL" $ field "user"
-- >   return (Config host port user)
--
-- Assuming that we generate lenses for the @Config@ type above,
-- then we can use the lens-based combinators in this module to
-- write terser parsing code by providing which lens to update
-- along with each field:
--
-- > configParser :: IniLensParser Config ()
-- > configParser = do
-- >   section "NETWORK" $ do
-- >     cfHost .= fieldOf "host" string
-- >     cfPort .= fieldOf "port" number
-- >   section "LOCAL" $ do
-- >     cfUser .= field "user"
--
-- One downside to this approach is that you need an existing
-- value of the configuration type to update, which might mean
-- filling in a dummy value with nonsense data, even for fields
-- which are obligatory in the configuration, but on the other
-- hand, this can make some parsing code much more flexible and
-- terse.
