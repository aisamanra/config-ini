{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Ini.Config.St
  ( -- $main
    parseIniFileSt,

    -- * Setter-Based Parser Types
    IniStParser,
    SectionStParser,

    -- * Setter-Based Section-Level Parsing
    sectionSt,
    sectionOptSt,

    -- * Setter-Aware Field-Level Parsing

    -- ** Using setter functions
    setterField,
    setterFieldOpt,

    -- ** Using lenses
    lensField,
    (.=),
    lensFieldOpt,
    (.=?),

    -- ** Setter-Based Field Parsing Aliases
    fieldSt,
    fieldOfSt,
    fieldMbSt,
    fieldMbOfSt,
    fieldOptSt,
    fieldOptOfSt,
    fieldDefSt,
    fieldDefOfSt,
    fieldFlagSt,
    fieldFlagDefSt,

    -- * Reader Functions
    Lens,
    updateLens,
    module Data.Ini.Config,
  )
where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict
import Data.Ini.Config
import Data.Monoid (Endo (..))
import Data.Text (Text)

-- $setup
-- >>> type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
--
-- >>> let lens get set f a = (`set` a) `fmap` f (get a)
--
-- >>> let _1 = lens fst (\ a (_, b) -> (a, b))
--
-- >>> let _2 = lens snd (\ b (a, _) -> (a, b))
--
-- >>> let set lens x a = fromI (lens (\ _ -> I x) a)

-- | This is a "lens"-compatible type alias
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- We need this to implement 'set' for lenses
newtype I a = I {fromI :: a}

instance Functor I where fmap f (I a) = I (f a)

set :: Lens s s a a -> a -> s -> s
set lens x a = fromI (lens (\_ -> I x) a)

-- | This is a function compatible with the @fieldOf@ family of functions. It allows
--   you to parse a field and then create an update function with it.
updateLens :: (Text -> Either String a) -> Lens s s a a -> Text -> Either String (s -> s)
updateLens rd lens text = do
  case rd text of
    Left err -> Left err
    Right r -> Right (\st -> set lens r st)

newtype IniStParser s a = IniStParser (WriterT (Endo s) IniParser a)
  deriving (Functor, Applicative, Alternative, Monad)

newtype SectionStParser s a = SectionStParser (WriterT (Endo s) SectionParser a)
  deriving (Functor, Applicative, Alternative, Monad)

parseIniFileSt :: Text -> s -> IniStParser s () -> Either String s
parseIniFileSt text def (IniStParser mote) = do
  (_, Endo update) <- parseIniFile text (runWriterT mote)
  return (update def)

sectionSt :: Text -> SectionStParser s () -> IniStParser s ()
sectionSt name (SectionStParser thunk) = IniStParser $ do
  ((), update) <- lift (section name (runWriterT thunk))
  tell update
  return ()

sectionOptSt :: Text -> SectionStParser s () -> IniStParser s ()
sectionOptSt name (SectionStParser thunk) = IniStParser $ do
  updateMb <- lift (sectionMb name (runWriterT thunk))
  case updateMb of
    Nothing -> return ()
    Just ((), update) -> tell update

liftSetter :: (a -> s -> s) -> SectionParser a -> SectionStParser s ()
liftSetter setter mote = SectionStParser $ do
  rs <- lift mote
  tell $ Endo (setter rs)

-- | The 'setterField' function turns a setter and a relevant 'SectionParser'
--   field into a 'SectionStParser' that uses the setter to update
--   an internal value using the result of the 'SectionParser'.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (setterField (set _1) (field "x"))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (setterField (set _1) (field "y"))
-- Left "Missing field \"y\" in section \"MAIN\""
setterField :: (a -> s -> s) -> SectionParser a -> SectionStParser s ()
setterField = liftSetter

-- | The 'setterFieldOpt' function turns a setter and a relevant 'SectionParser'
--   field into a 'SectionStParser' that uses the setter to update an internal
--   value with the 'Just' result from the 'SectionParser', and does nothing
--   if the 'SectionParser' returns 'Nothing'.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (setterFieldOpt (set _1) (fieldMb "x"))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (setterFieldOpt (set _1) (fieldMb "y"))
-- Right ("def",False)
setterFieldOpt :: (a -> s -> s) -> SectionParser (Maybe a) -> SectionStParser s ()
setterFieldOpt setter mote = SectionStParser $ do
  rsMb <- lift mote
  case rsMb of
    Just rs -> tell $ Endo (setter rs)
    Nothing -> return ()

-- | The 'lensField' function (or its operator form '.=') turns a lens and a
--   standard 'SectionParser' field into a 'SectionStParser' that uses the
--   lens to update an internal value to the result of the
--   'SectionParser'.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (lensField _1 (field "x"))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (lensField _1 (field "y"))
-- Left "Missing field \"y\" in section \"MAIN\""
lensField :: Lens s s a a -> SectionParser a -> SectionStParser s ()
lensField lens mote = SectionStParser $ do
  rs <- lift mote
  tell $ Endo (set lens rs)

-- | An infix alias for 'lensField'.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (_1 .= field "x")
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (_1 .= field "y")
-- Left "Missing field \"y\" in section \"MAIN\""
(.=) :: Lens s s a a -> SectionParser a -> SectionStParser s ()
(.=) = lensField

-- | The 'lensFieldOpt' function (or its operator form '.=?') turns a lens
--   and a standard 'SectionParser' field into a 'SectionStParser' that
--   ignores values that are not present, but uses the lens to set a value
--   that is present.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (lensFieldOpt _1 (fieldMb "x"))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (lensFieldOpt _1 (fieldMb "y"))
-- Right ("def",False)
lensFieldOpt :: Lens s s a a -> SectionParser (Maybe a) -> SectionStParser s ()
lensFieldOpt lens mote = SectionStParser $ do
  rsMb <- lift mote
  case rsMb of
    Just rs -> tell $ Endo (set lens rs)
    Nothing -> return ()

-- | An infix alias for 'lensFieldOpt'.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (_1 .=? fieldMb "x")
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (_1 .=? fieldMb "y")
-- Right ("def",False)
(.=?) :: Lens s s a a -> SectionParser (Maybe a) -> SectionStParser s ()
(.=?) = lensFieldOpt

-- | A setter-aware variant of 'field': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldSt "x" (set _1))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldSt "y" (set _1))
-- Left "Missing field \"y\" in section \"MAIN\""
fieldSt :: Text -> (Text -> s -> s) -> SectionStParser s ()
fieldSt name setter = liftSetter setter $ field name

-- | A setter-aware variant of 'fieldOf': the setter argument names the
--   setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldOfSt "x" number (set _1))
-- Right (72,False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (0, False) $ sectionSt "MAIN" (fieldOfSt "x" number (set _1))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldOfSt "y" number (set _1))
-- Left "Missing field \"y\" in section \"MAIN\""
fieldOfSt :: Text -> (Text -> Either String a) -> (a -> s -> s) -> SectionStParser s ()
fieldOfSt name rd setter = liftSetter setter $ fieldOf name rd

-- | A setter-aware variant of 'fieldMb': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (Just "def", False) $ sectionSt "MAIN" (fieldMbSt "x" (set _1))
-- Right (Just "hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (Just "def", False) $ sectionSt "MAIN" (fieldMbSt "y" (set _1))
-- Right (Nothing,False)
fieldMbSt :: Text -> (Maybe Text -> s -> s) -> SectionStParser s ()
fieldMbSt name setter = liftSetter setter $ fieldMb name

-- | A setter-aware variant of 'fieldMbOf': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (Just 0, False) $ sectionSt "MAIN" (fieldMbOfSt "x" number (set _1))
-- Right (Just 72,False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (Just 0, False) $ sectionSt "MAIN" (fieldMbOfSt "x" number (set _1))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (Just 0, False) $ sectionSt "MAIN" (fieldMbOfSt "y" number (set _1))
-- Right (Nothing,False)
fieldMbOfSt :: Text -> (Text -> Either String a) -> (Maybe a -> s -> s) -> SectionStParser s ()
fieldMbOfSt name rd setter = liftSetter setter $ fieldMbOf name rd

-- | A setter-aware variant of 'field' which does nothing if a key
--   is absent. The setter argument names the setter to use on the
--   underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldOptSt "x" (set _1))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldOptSt "y" (set _1))
-- Right ("def",False)
fieldOptSt :: Text -> (Text -> s -> s) -> SectionStParser s ()
fieldOptSt name setter = SectionStParser $ do
  rsMb <- lift (fieldMb name)
  case rsMb of
    Nothing -> return ()
    Just rs -> tell $ Endo (setter rs)

-- | A setter-aware variant of 'fieldOf', which does nothing if a key
--  is absent. The setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldOptOfSt "x" number (set _1))
-- Right (72,False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (0, False) $ sectionSt "MAIN" (fieldOptOfSt "x" number (set _1))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldOptOfSt "y" number (set _1))
-- Right (0,False)
fieldOptOfSt :: Text -> (Text -> Either String a) -> (a -> s -> s) -> SectionStParser s ()
fieldOptOfSt name rd setter = SectionStParser $ do
  rsMb <- lift (fieldMbOf name rd)
  case rsMb of
    Nothing -> return ()
    Just rs -> tell $ Endo (setter rs)

-- | A setter-aware variant of 'fieldDef': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("orig", False) $ sectionSt "MAIN" (fieldDefSt "x" "def" (set _1))
-- Right ("hello",False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("orig", False) $ sectionSt "MAIN" (fieldDefSt "y" "def" (set _1))
-- Right ("def",False)
fieldDefSt :: Text -> Text -> (Text -> s -> s) -> SectionStParser s ()
fieldDefSt name def setter = liftSetter setter $ fieldDef name def

-- | A setter-aware variant of 'fieldDefOf': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldDefOfSt "x" number 99 (set _1))
-- Right (72,False)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" (0, False) $ sectionSt "MAIN" (fieldDefOfSt "x" number 99 (set _1))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a value of type Integer"
-- >>> parseIniFileSt "[MAIN]\nx = 72\n" (0, False) $ sectionSt "MAIN" (fieldDefOfSt "y" number 99 (set _1))
-- Right (99,False)
fieldDefOfSt :: Text -> (Text -> Either String a) -> a -> (a -> s -> s) -> SectionStParser s ()
fieldDefOfSt name rd def setter = liftSetter setter $ fieldDefOf name rd def

-- | A setter-aware variant of 'fieldFlag': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = yes\n" ("def", False) $ sectionSt "MAIN" (fieldFlagSt "x" (set _2))
-- Right ("def",True)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldFlagSt "x" (set _2))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a boolean"
-- >>> parseIniFileSt "[MAIN]\nx = yes\n" ("def", False) $ sectionSt "MAIN" (fieldFlagSt "y" (set _2))
-- Left "Missing field \"y\" in section \"MAIN\""
fieldFlagSt :: Text -> (Bool -> s -> s) -> SectionStParser s ()
fieldFlagSt name setter = liftSetter setter $ fieldFlag name

-- | A setter-aware variant of 'fieldFlagDef': the setter argument names the
--  setter to use on the underlying value being modified.
--
-- >>> parseIniFileSt "[MAIN]\nx = yes\n" ("def", False) $ sectionSt "MAIN" (fieldFlagDefSt "x" False (set _2))
-- Right ("def",True)
-- >>> parseIniFileSt "[MAIN]\nx = hello\n" ("def", False) $ sectionSt "MAIN" (fieldFlagDefSt "x" False (set _2))
-- Left "Line 2, in section \"MAIN\": Unable to parse \"hello\" as a boolean"
-- >>> parseIniFileSt "[MAIN]\nx = yes\n" ("def", False) $ sectionSt "MAIN" (fieldFlagDefSt "y" False (set _2))
-- Right ("def",False)
fieldFlagDefSt :: Text -> Bool -> (Bool -> s -> s) -> SectionStParser s ()
fieldFlagDefSt name def setter = liftSetter setter $ fieldFlagDef name def

-- $main
-- This module is designed to be used with update functions
-- like setters or leneses, so that we can
-- start with a default configuration and gradually update it,
-- rather than construct a new value from scratch. Among other
-- things, this introduces more flexibility in terms of how we
-- organize both the configuration file and the data type that
-- represents the configuration. Consider the same example code
-- that appears in the documentation for the "Data.Ini.Config"
-- module, which parses a configuration file like this:
--
-- > [NETWORK]
-- > host = example.com
-- > port = 7878
-- >
-- > [LOCAL]
-- > user = terry
--
-- In that example, we split the configuration into a @NetworkConfig@
-- and a @LocalConfig@ type to mirror the configuration file's use of
-- @[LOCAL]@ and @[NETWORK]@ sections, but we might want to keep the
-- configuration data type as a single flat record, in which case our
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
-- > configParser :: IniStParser Config ()
-- > configParser = do
-- >   sectionSt "NETWORK" $ do
-- >     cfHost .= fieldOf "host" string
-- >     cfPort .= fieldOf "port" number
-- >   sectionSt "LOCAL" $ do
-- >     cfUser .= field "user"
--
-- One downside to this approach is that you need an existing
-- value of the configuration type to update, which might mean
-- filling in a dummy value with nonsense data, even for fields
-- which are obligatory in the configuration but on the other
-- hand, this can make some parsing code much more flexible and
-- terse.
