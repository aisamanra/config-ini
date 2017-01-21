{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ini.Config.Bidir
(
  parseIniFile
, emitIniFile
-- * Bidirectional Parser Types
, IniParser
, SectionParser
-- * Section-Level Parsing
, section
-- * Field-Level Parsing
, (.=)
, (.=?)
, (<?>)
, field
, fieldOf
, fieldDef
, fieldDefOf
, flag
, flagDef
-- * FieldValues
, FieldValue(..)
, text
, string
, number
, bool
, readable
) where

import           Control.Monad.Trans.State.Strict (State, runState, modify)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, Proxy(..), typeRep)
import           Text.Read (readMaybe)

import           Data.Ini.Config.Raw

-- | This is a "lens"-compatible type alias
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lkp :: Text -> Seq (Text, a) -> Maybe a
lkp t = go . Seq.viewl
  where go ((t', x) Seq.:< rs)
          | t == t'   = Just x
          | otherwise = go (Seq.viewl rs)
        go Seq.EmptyL = Nothing

data FieldValue a = FieldValue
  { fvParse :: Text -> Either String a
  , fvEmit  :: a -> Text
  }

data OutputOptions = OutputOptions
  { outputOrdering :: OutputOrdering
  } deriving (Eq, Show)

data OutputOrdering
  = SameAsSpecification
  | SameAsInputFile
    deriving (Eq, Show)

type BidirM s a = State (Seq s) a

runBidirM :: BidirM s a -> Seq s
runBidirM = snd . flip runState Seq.empty

newtype IniParser s a = IniParser (BidirM (Text, Seq (Field s)) a)
  deriving (Functor, Applicative, Monad)

newtype SectionParser s a = SectionParser (BidirM (Field s) a)
  deriving (Functor, Applicative, Monad)

section :: Text -> SectionParser s () -> IniParser s ()
section name (SectionParser mote) = IniParser $ do
  let fields = runBidirM mote
  modify (Seq.|> (name, fields))

data Field s
  = forall a. Field (Lens s s a a) (FieldDescription a) Bool

data FieldDescription t = FieldDescription
  { fdName    :: Text
  , fdValue   :: FieldValue t
  , fdDefault :: Maybe t
  , fdComment :: Seq Text
  }

(.=) :: Lens s s t t -> FieldDescription t -> SectionParser s ()
l .= f = SectionParser $ modify (Seq.|> fd)
  where fd = Field l f False

(.=?) :: Lens s s t t -> FieldDescription t -> SectionParser s ()
l .=? f = SectionParser $ modify (Seq.|> fd)
  where fd = Field l f True

(<?>) :: FieldDescription t -> [Text] -> FieldDescription t
fd <?> comment = fd { fdComment = Seq.fromList comment }

infixr 8 .=
infixr 8 .=?
infixr 9 <?>

field :: Text -> FieldDescription Text
field name = fieldOf name text

fieldOf :: Text -> FieldValue a -> FieldDescription a
fieldOf name value = FieldDescription
  { fdName    = name
  , fdValue   = value
  , fdDefault = Nothing
  , fdComment = Seq.empty
  }

fieldDef :: Text -> Text -> FieldDescription Text
fieldDef name def = fieldDefOf name def text

fieldDefOf :: Text -> a -> FieldValue a -> FieldDescription a
fieldDefOf name def value = FieldDescription
  { fdName    = name
  , fdValue   = value
  , fdDefault = Just def
  , fdComment = Seq.empty
  }

flag :: Text -> FieldDescription Bool
flag name = fieldOf name bool

flagDef :: Text -> Bool -> FieldDescription Bool
flagDef name def = fieldDefOf name def bool

sample :: SectionParser (Text, Int) ()
sample = do
  _1 .= field "foo" <?> ["comment for foo"]
  _2 .= fieldDefOf "bar" 0 number

readable :: forall a. (Show a, Read a, Typeable a) => FieldValue a
readable = FieldValue { fvParse = parse, fvEmit = emit }
  where emit = T.pack . show
        parse t = case readMaybe (T.unpack t) of
          Just v -> Right v
          Nothing -> Left ("Unable to parse " ++ show t ++
                           " as a value of type " ++ show typ)
        typ = typeRep (prx)
        prx :: Proxy a
        prx = Proxy

number :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number = readable

text :: FieldValue Text
text = FieldValue { fvParse = Right, fvEmit = id }

string :: FieldValue String
string = FieldValue { fvParse = Right . T.unpack, fvEmit = T.pack }

bool :: FieldValue Bool
bool = FieldValue { fvParse = parse, fvEmit = emit }
  where parse s = case T.toLower s of
          "true"  -> Right True
          "yes"   -> Right True
          "t"     -> Right True
          "y"     -> Right True
          "false" -> Right False
          "no"    -> Right False
          "f"     -> Right False
          "n"     -> Right False
          _       -> Left ("Unable to parse " ++ show s ++ " as a boolean")
        emit True  = "true"
        emit False = "false"

parseIniFile :: s -> IniParser s () -> Text -> Either String s
parseIniFile def (IniParser mote) t =
  let spec = runBidirM mote
  in case parseIni t of
    Left err        -> Left err
    Right (Ini ini) -> runSpec def (Seq.viewl spec) ini

runSpec :: s -> Seq.ViewL (Text, Seq (Field s)) -> Seq (Text, IniSection) -> Either String s
runSpec s Seq.EmptyL _ = Right s
runSpec s ((name, fs) Seq.:< rest) ini
  | Just v <- lkp (T.toLower name) ini = do
      s' <- runFields s (Seq.viewl fs) v
      runSpec s' (Seq.viewl rest) ini
  | otherwise = Left ("Unable to find section " ++ show name)

newtype I a = I { fromI :: a }
instance Functor I where fmap f (I x) = I (f x)

set :: Lens s t a b -> b -> s -> t
set lns x a = fromI (lns (const (I x)) a)

newtype C a b = C { fromC :: a }
instance Functor (C a) where fmap _ (C x) = C x

get :: Lens s t a b -> s -> a
get lns a = fromC (lns C a)

runFields :: s -> Seq.ViewL (Field s) -> IniSection -> Either String s
runFields s Seq.EmptyL _ = Right s
runFields s (Field l descr optional Seq.:< fs) sect
  | Just v <- lkp (fdName descr) (isVals sect) = do
      value <- fvParse (fdValue descr) (vValue v)
      runFields (set l value s) (Seq.viewl fs) sect
  | Just def <- fdDefault descr =
      runFields (set l def s) (Seq.viewl fs) sect
  | optional =
      runFields s (Seq.viewl fs) sect
  | otherwise = Left ("Unable to find field " ++ show (fdName descr))

emitIniFile :: s -> IniParser s () -> Text
emitIniFile s (IniParser mote) =
  let spec = runBidirM mote in
  printIni $ Ini $ fmap (\ (name, fs) -> (name, toSection s name fs)) spec

toSection :: s -> Text -> Seq (Field s) -> IniSection
toSection s name fs = IniSection
  { isName = name
  , isVals = fmap toVal fs
  , isStartLine = 0
  , isEndLine   = 0
  , isComments  = Seq.empty
  } where toVal (Field l descr optional) =
            ( fdName descr
            , IniValue
                { vLineNo   = 0
                , vName     = fdName descr
                , vValue    = fvEmit (fdValue descr) (get l s)
                , vComments = BlankLine Seq.<|
                                fmap (\ ln -> CommentLine '#' (" " <> ln))
                                     (fdComment descr)
                , vCommentedOut = optional
                }
            )

-- DELETE ME LATER

lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens gt st f a = (`st` a) `fmap` f (gt a)

_1 :: Lens (a, b) (a, b) a a
_1 = lens fst (\ a (_, b) -> (a, b))

_2 :: Lens (a, b) (a, b) b b
_2 = lens snd (\ b (a, _) -> (a, b))


-- $main
-- This module is an alternate API used for parsing INI files.
-- unlike the standard API, it is bidirectional: it can be
-- used to emit an INI or even produce a modified INI file
-- with minimal modification.
--
-- This module is designed to be used with lenses: instead of
-- generating a new value as a result of parsing, we start
-- with a fully constructed value and then associate each field
-- of the INI file with a lens into that structure. Among other
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
-- > configSpec :: IniSpec Config ()
-- > configSpec = do
-- >   sectionSt "NETWORK" $ do
-- >     cfHost .= field   "host" string
-- >     cfPort .= fieldOf "port" number
-- >   sectionSt "LOCAL" $ do
-- >     cfUser .= field "user"
--
-- Additionally, given a value of type @Config@, we can use the
-- same specification to emit an INI file, which is useful for
-- generating a valid sample configuration. To help with this,
-- we can rewrite the spec to make use of the @<?>@ operator,
-- which associates the field with a comment block that will
-- be printed above the field in the generated INI file.
--
-- > configSpec :: IniSpec Config ()
-- > configSpec = do
-- >   sectionSt "NETWORK" $ do
-- >     cfHost .= field   "host" string <?> [ "the host" ]
-- >     cfPort .= fieldOf "port" number <?> [ "the port" ]
-- >   sectionSt "LOCAL" $ do
-- >     cfUser .= field "user" <?> [ "the username" ]
--
-- Using an existing @Config@ value here, we can produce a sample
-- configuration:
