{-|
Module     : Data.Ini.Config.Bidir
Copyright  : (c) Getty Ritter, 2017
License    : BSD
Maintainer : Getty Ritter <config-ini@infinitenegativeutility.com>
Stability  : experimental

This module presents an alternate API for parsing INI files.  Unlike
the standard API, it is bidirectional: the same declarative structure
can be used to parse an INI file to a value, serialize an INI file
from a value, or even /update/ an INI file by comparing it against a
value and serializing in a way that minimizes the differences between
revisions of the file.

This API does make some extra assumptions about your configuration
type and the way you interact with it: in particular, it assumes that
you have lenses for all the fields you're parsing and that you have
some kind of sensible default value of that configuration
type. Instead of providing combinators which can extract and parse a
field of an INI file into a value, the bidirectional API allows you to
declaratively associate a lens into your structure with a field of the
INI file.

Consider the following example INI file:

> [NETWORK]
> host = example.com
> port = 7878
>
> [LOCAL]
> user = terry

We'd like to parse this INI file into a @Config@ type which we've
defined like this, using
<https://hackage.haskell.org/package/lens lens> or a similar library
to provide lenses:

> data Config = Config
>   { _cfHost :: String
>   , _cfPort :: Int
>   , _cfUser :: Maybe Text
>   } deriving (Eq, Show)
>
> ''makeLenses Config

We can now define a basic specification of the type @'IniSpec' Config
()@ by using the provided operations to declare our top-level
sections, and then within those sections we can associate fields with
@Config@ lenses.

@
'configSpec' :: 'IniSpec' Config ()
'configSpec' = do
  'section' \"NETWORK\" $ do
    cfHost '.=' 'field' \"host\" 'string'
    cfPost '.=' 'field' \"port\" 'number'
  'sectionOpt' \"LOCAL\" $ do
    cfUser '.=?' 'field' \"user\" 'text'
@

There are two operators used to associate lenses with fields:

['.='] Associates a lens of type @Lens' s a@ with a field description
       of type @FieldDescription a@. By default, this will raise an
       error when parsing if the field described is missing, but we
       can mark it as optional, as we'll see.

['.=?'] Associates a lens of type @Lens' s (Maybe a)@ with a field
        description of type @FieldDescription a@. During parsing, if
        the value does not appear in an INI file, then the lens will
        be set to 'Nothing'; similarly, during serializing, if the
        value is 'Nothing', then the field will not be serialized in
        the file.

Each field must include the field's name as well as a 'FieldValue',
which describes how to both parse and serialize a value of a given
type. Several built-in 'FieldValue' descriptions are provided, but you
can always build your own by providing parsing and serialization
functions for individual fields.

We can also provide extra metadata about a field, allowing it to be
skipped durin parsing, or to provide an explicit default value, or to
include an explanatory comment for that value to be used when we
serialize an INI file. These are conventionally applied to the field
using the '&' operator:

@
configSpec :: 'IniSpec' Config ()
configSpec = do
  'section' \"NETWORK\" $ do
    cfHost '.=' 'field' \"host\" 'string'
                & 'comment' [\"The desired hostname (optional)\"]
                & 'optional'
    cfPost '.=' 'field' \"port\" 'number'
                & 'comment' [\"The port number\"]
  'sectionOpt' \"LOCAL\" $ do
    cfUser '.=?' 'field' \"user\" 'text'
@

When we want to use this specification, we need to create a value of
type 'Ini', which is an abstract representation of an INI
specification. To create an 'Ini' value, we need to use the 'ini'
function, which combines the spec with the default version of our
configuration value.

Once we have a value of type 'Ini', we can use it for three basic
operations:

* We can parse a textual INI file with 'parseIni', which will
  systematically walk the spec and use the provided lens/field
  associations to create a parsed configuration file. This will give
  us a new value of type 'Ini' that represents the parsed
  configuration, and we can extract the actual configuration value
  with 'getIniValue'.

* We can update the value contained in an 'Ini' value. If the 'Ini'
  value is the result of a previous call to 'parseIni', then this
  update will attempt to retain as much of the incidental structure of
  the parsed file as it can: for example, it will attempt to retain
  comments, whitespace, and ordering. The general strategy is to make
  the resulting INI file "diff-minimal": the diff between the older
  INI file and the updated INI file should contain as little noise as
  possible. Small cosmetic choices such as how to treat generated
  comments are controlled by a configurable 'UpdatePolicy' value.

* We can serialize an 'Ini' value to a textual INI file. This will
  produce the specified INI file (either a default fresh INI, or a
  modified existing INI) as a textual value.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Ini.Config.Bidir
(
-- * Parsing, Serializing, and Updating Files
-- $using
  Ini
, ini
, getIniValue
, iniValueL
, getRawIni
-- ** Parsing INI files
, parseIni
-- ** Serializing INI files
, serializeIni
-- ** Updating INI Files
, updateIni
, setIniUpdatePolicy
, UpdatePolicy(..)
, UpdateCommentPolicy(..)
, defaultUpdatePolicy
-- * Bidirectional Parser Types
-- $types
, IniSpec
, SectionSpec

-- * Section-Level Parsing
-- $sections
, section
, allOptional

-- * Field-Level Parsing
-- $fields
, FieldDescription
, (.=)
, (.=?)
, field
, flag
, comment
, placeholderValue
, optional

-- * FieldValues
-- $fieldvalues
, FieldValue(..)
, text
, string
, number
, bool
, readable
, listWithSeparator
, pairWithSeparator

-- * Miscellaneous Helpers
-- $misc
, (&)
, Lens

) where

import           Control.Monad.Trans.State.Strict (State, runState, modify)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as F
#if __GLASGOW_HASKELL__ >= 710
import           Data.Function ((&))
#endif
import           Data.Monoid ((<>))
import           Data.Sequence ((<|), Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Traversable as F
import           Data.Typeable (Typeable, Proxy(..), typeRep)
import           GHC.Exts (IsList(..))
import           Text.Read (readMaybe)

import           Data.Ini.Config.Raw

-- * Utility functions + lens stuffs

-- | This is a
--   <https://hackage.haskell.org/package/lens lens>-compatible
--   type alias
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- These are some inline reimplementations of "lens" operators. We
-- need the identity functor to implement 'set':
newtype I a = I { fromI :: a }
instance Functor I where fmap f (I x) = I (f x)

set :: Lens s t a b -> b -> s -> t
set lns x a = fromI (lns (const (I x)) a)

-- ... and we need the const functor to implement 'get':
newtype C a b = C { fromC :: a }
instance Functor (C a) where fmap _ (C x) = C x

get :: Lens s t a b -> s -> a
get lns a = fromC (lns C a)

lkp :: NormalizedText -> Seq (NormalizedText, a) -> Maybe a
lkp t = fmap snd . F.find (\ (t', _) -> t' == t)

rmv :: NormalizedText -> Seq (Field s) -> Seq (Field s)
rmv n = Seq.filter (\ f -> fieldName f /= n)

-- The & operator is really useful here, but it didn't show up in
-- earlier versions, so it gets redefined here.
#if __GLASGOW_HASKELL__ < 710
{- | '&' is a reverse application operator. This provides notational
     convenience. Its precedence is one higher than that of the
     forward application operator '$', which allows '&' to be nested
     in '$'. -}
(&) :: a -> (a -> b) -> b
a & f = f a
infixl 1 &
#endif

-- * The 'Ini' type

-- | An 'Ini' is an abstract representation of an INI file, including
-- both its textual representation and the Haskell value it
-- represents.
data Ini s = Ini
  { iniSpec :: Spec s
  , iniCurr :: s
  , iniDef  :: s
  , iniLast :: Maybe RawIni
  , iniPol  :: UpdatePolicy
  }

-- | Create a basic 'Ini' value from a default value and a spec.
ini :: s -> IniSpec s () -> Ini s
ini def (IniSpec spec) = Ini
  { iniSpec = runBidirM spec
  , iniCurr = def
  , iniDef  = def
  , iniLast = Nothing
  , iniPol  = defaultUpdatePolicy
  }

-- | Get the underlying Haskell value associated with the 'Ini'.
getIniValue :: Ini s -> s
getIniValue = iniCurr

mkLens :: (a -> b) -> (b -> a -> a) -> Lens a a b b
mkLens get' set' f a = (`set'` a) `fmap` f (get' a)

-- | The lens equivalent of 'getIniValue'
iniValueL :: Lens (Ini s) (Ini s) s s
iniValueL = mkLens iniCurr (\ i v -> v { iniCurr = i })

-- | Get the textual representation of an 'Ini' value. If this 'Ini'
-- value is the result of 'parseIni', then it will attempt to retain
-- the textual characteristics of the parsed version as much as
-- possible (e.g. by retaining comments, ordering, and whitespace in a
-- way that will minimize the overall diff footprint.) If the 'Ini'
-- value was created directly from a value and a specification, then
-- it will pretty-print an initial version of the file with the
-- comments and placeholder text specified in the spec.
serializeIni :: Ini s -> Text
serializeIni = printRawIni . getRawIni

-- | Get the underlying 'RawIni' value for the file.
getRawIni :: Ini s -> RawIni
getRawIni (Ini { iniLast = Just raw }) = raw
getRawIni (Ini { iniCurr = s
               , iniSpec = spec
               }) = emitIniFile s spec

-- | Parse a textual representation of an 'Ini' file. If the file is
-- malformed or if an obligatory field is not found, this will produce
-- a human-readable error message. If an optional field is not found,
-- then it will fall back on the existing value contained in the
-- provided 'Ini' structure.
parseIni :: Text -> Ini s -> Either String (Ini s)
parseIni t i@Ini { iniSpec = spec
                 , iniCurr = def
                 } = do
  RawIni raw <- parseRawIni t
  s <- parseSections def (Seq.viewl spec) raw
  return $ i
    { iniCurr = s
    , iniLast = Just (RawIni raw)
    }

-- | Update the internal value of an 'Ini' file. If this 'Ini' value
-- is the result of 'parseIni', then the resulting 'Ini' value will
-- attempt to retain the textual characteristics of the parsed version
-- as much as possible (e.g. by retaining comments, ordering, and
-- whitespace in a way that will minimize the overall diff footprint.)
updateIni :: s -> Ini s -> Ini s
updateIni new i =
  case doUpdateIni new i of
    Left err -> error err
    Right i' -> i'

-- | Use the provided 'UpdatePolicy' as a guide when creating future
-- updated versions of the given 'Ini' value.
setIniUpdatePolicy :: UpdatePolicy -> Ini s -> Ini s
setIniUpdatePolicy pol i = i { iniPol = pol }

-- * Type definitions

-- | A value of type 'FieldValue' packages up a parser and emitter
--   function into a single value. These are used for bidirectional
--   parsing and emitting of the value of a field.
data FieldValue a = FieldValue
  { fvParse :: Text -> Either String a
    -- ^ The function to use when parsing the value of a field; if
    --   the parser fails, then the string will be shown as an error
    --   message to the user.
  , fvEmit  :: a -> Text
    -- ^ The function to use when serializing a value into an INI
    -- file.
  }

-- This is actually being used as a writer monad, but using a state
-- monad lets us avoid the space leaks. Not that those are likely to
-- be a problem in this application, but it's not like it cost us
-- none.
type BidirM s a = State (Seq s) a

runBidirM :: BidirM s a -> Seq s
runBidirM = snd . flip runState Seq.empty

type Spec s = Seq (Section s)

-- | An 'IniSpec' value represents the structure of an entire
-- INI-format file in a declarative way. The @s@ parameter represents
-- the type of a Haskell structure which is being serialized to or
-- from.
newtype IniSpec s a = IniSpec (BidirM (Section s) a)
  deriving (Functor, Applicative, Monad)

-- | A 'SectionSpec' value represents the structure of a single
-- section of an INI-format file in a declarative way. The @s@
-- parameter represents the type of a Haskell structure which is being
-- serialized to or from.
newtype SectionSpec s a = SectionSpec (BidirM (Field s) a)
  deriving (Functor, Applicative, Monad)

-- * Sections

-- | Define the specification of a top-level INI section.
section :: Text -> SectionSpec s () -> IniSpec s ()
section name (SectionSpec mote) = IniSpec $ do
  let fields = runBidirM mote
  modify (Seq.|> Section (normalize name) fields (allFieldsOptional fields))

allFieldsOptional :: (Seq (Field s)) -> Bool
allFieldsOptional = all isOptional
  where isOptional (Field   _ fd) = fdSkipIfMissing fd
        isOptional (FieldMb _ _)  = True

-- | Treat an entire section as containing entirely optional fields.
allOptional
  :: (SectionSpec s () -> IniSpec s ())
  -> (SectionSpec s () -> IniSpec s ())
allOptional k spec = IniSpec $ do
  let IniSpec comp = k spec
  comp
  modify (\ s -> case Seq.viewr s of
             EmptyR -> s
             rs :> Section name fields _ ->
               rs Seq.|> Section name (fmap makeOptional fields) True)

makeOptional :: Field s -> Field s
makeOptional (Field l d) = Field l d { fdSkipIfMissing = True }
makeOptional (FieldMb l d) = FieldMb l d { fdSkipIfMissing = True }

data Section s = Section NormalizedText (Seq (Field s)) Bool

-- * Fields

-- | A "Field" is a description of
data Field s
  = forall a. Eq a => Field (Lens s s a a) (FieldDescription a)
  | forall a. Eq a => FieldMb (Lens s s (Maybe a) (Maybe a)) (FieldDescription a)

-- convenience accessors for things in a Field
fieldName :: Field s -> NormalizedText
fieldName (Field _ FieldDescription { fdName = n }) = n
fieldName (FieldMb _ FieldDescription { fdName = n }) = n

fieldComment :: Field s -> Seq Text
fieldComment (Field _ FieldDescription { fdComment = n }) = n
fieldComment (FieldMb _ FieldDescription { fdComment = n }) = n

-- | A 'FieldDescription' is a declarative representation of the
-- structure of a field. This includes the name of the field and the
-- 'FieldValue' used to parse and serialize values of that field, as
-- well as other metadata that might be needed in the course of
-- parsing or serializing a structure.
data FieldDescription t = FieldDescription
  { fdName          :: NormalizedText
  , fdValue         :: FieldValue t
  , fdComment       :: Seq Text
  , fdDummy         :: Maybe Text
  , fdSkipIfMissing :: Bool
  }

-- ** Field operators

{- |
Associate a field description with a field. If this field
is not present when parsing, it will attempt to fall back
on a default, and if no default value is present, it will
fail to parse.

When serializing an INI file, this will produce all the
comments associated with the field description followed
by the value of the field in the.
-}
(.=) :: Eq t => Lens s s t t -> FieldDescription t -> SectionSpec s ()
l .= f = SectionSpec $ modify (Seq.|> fd)
  where fd = Field l f

{- |
Associate a field description with a field of type "Maybe a".
When parsing, this field will be initialized to "Nothing" if
it is not found, and to a "Just" value if it is. When
serializing an INI file, this will try to serialize a value
-}
(.=?) :: Eq t => Lens s s (Maybe t) (Maybe t) -> FieldDescription t -> SectionSpec s ()
l .=? f = SectionSpec $ modify (Seq.|> fd)
  where fd = FieldMb l f

-- ** Field metadata

{- |
Associate a multiline comment with a "FieldDescription". When
serializing a field that has a comment associated, the comment will
appear before the field.
-}
comment :: [Text] -> FieldDescription t -> FieldDescription t
comment cmt fd = fd { fdComment = Seq.fromList cmt }

-- | Choose a placeholder value to be displayed for optional fields.
--   This is used when serializing an optional Ini field: the
--   field will appear commented out in the output using the
--   placeholder text as a value, so a spec that includes
--
--   @
--   myLens .=? field "x" & placeholderValue "\<val\>"
--   @
--
--   will serialize into an INI file that contains the line
--
--   @
--   # x = \<val\>
--   @
--
--   A placeholder value will only appear in the serialized output if
--   the field is optional, but will be preferred over serializing the
--   default value for an optional field. This will not affect INI
--   file updates.
placeholderValue :: Text -> FieldDescription t -> FieldDescription t
placeholderValue t fd = fd { fdDummy = Just t }

-- | If the field is not found in parsing, simply skip instead of
--   raising an error or setting anything.
optional :: FieldDescription t -> FieldDescription t
optional fd = fd { fdSkipIfMissing = True }

infixr 0 .=
infixr 0 .=?

-- ** Creating fields

-- | Create a description of a field by a combination of the name of
--   the field and a "FieldValue" describing how to parse and emit
--   values associated with that field.
field :: Text -> FieldValue a -> FieldDescription a
field name value = FieldDescription
  { fdName          = normalize (name <> " ")
  , fdValue         = value
  , fdComment       = Seq.empty
  , fdDummy         = Nothing
  , fdSkipIfMissing = False
  }

-- | Create a description of a 'Bool'-valued field.
flag :: Text -> FieldDescription Bool
flag name = field name bool

-- ** FieldValues

-- | A "FieldValue" for parsing and serializing values according to
--   the logic of the "Read" and "Show" instances for that type,
--   providing a convenient human-readable error message if the
--   parsing step fails.
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

-- | Represents a numeric field whose value is parsed according to the
-- 'Read' implementation for that type, and is serialized according to
-- the 'Show' implementation for that type.
number :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number = readable

-- | Represents a field whose value is a 'Text' value
text :: FieldValue Text
text = FieldValue { fvParse = Right, fvEmit = id }

-- | Represents a field whose value is a 'String' value
string :: FieldValue String
string = FieldValue { fvParse = Right . T.unpack, fvEmit = T.pack }

-- | Represents a field whose value is a 'Bool' value. This parser is
-- case-insensitive, and matches the words @true@, @false@, @yes@, and
-- @no@, as well as single-letter abbreviations for all of the
-- above. This will serialize as @true@ for 'True' and @false@ for
-- 'False'.
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

-- | Represents a field whose value is a sequence of other values
-- which are delimited by a given string, and whose individual values
-- are described by another 'FieldValue' value. This uses GHC's
-- `IsList` typeclass to convert back and forth between sequence
-- types.
listWithSeparator :: IsList l => Text -> FieldValue (Item l) -> FieldValue l
listWithSeparator sep fv = FieldValue
  { fvParse = fmap fromList . mapM (fvParse fv . T.strip) . T.splitOn sep
  , fvEmit  = T.intercalate sep . map (fvEmit fv) . toList
  }

-- | Represents a field whose value is a pair of two other values
-- separated by a given string, whose individual values are described
-- by two different 'FieldValue' values.
pairWithSeparator :: FieldValue l -> Text -> FieldValue r -> FieldValue (l, r)
pairWithSeparator left sep right = FieldValue
  { fvParse = \ t ->
      let (leftChunk, rightChunk) = T.breakOn sep t
      in do
        x <- fvParse left leftChunk
        y <- fvParse right rightChunk
        return (x, y)
  , fvEmit = \ (x, y) -> fvEmit left x <> sep <> fvEmit right y
  }

-- * Parsing INI files

-- Are you reading this source code? It's not even that gross
-- yet. Just you wait. This is just the regular part. 'runSpec' is
-- easy: we walk the spec, and for each section, find the
-- corresponding section in the INI file and call runFields.
parseSections
  :: s
  -> Seq.ViewL (Section s)
  -> Seq (NormalizedText, IniSection)
  -> Either String s
parseSections s Seq.EmptyL _ = Right s
parseSections s (Section name fs opt Seq.:< rest) i
  | Just v <- lkp name i = do
      s' <- parseFields s (Seq.viewl fs) v
      parseSections s' (Seq.viewl rest) i
  | opt = parseSections s (Seq.viewl rest) i
  | otherwise = Left ("Unable to find section " ++
                      show (normalizedText name))

-- Now that we've got 'set', we can walk the field descriptions and
-- find them. There's some fiddly logic, but the high-level idea is
-- that we try to look up a field, and if it exists, parse it using
-- the provided parser and use the provided lens to add it to the
-- value. We have to decide what to do if it's not there, which
-- depends on lens metadata and whether it's an optional field or not.
parseFields :: s -> Seq.ViewL (Field s) -> IniSection -> Either String s
parseFields s Seq.EmptyL _ = Right s
parseFields s (Field l descr Seq.:< fs) sect
  | Just v <- lkp (fdName descr) (isVals sect) = do
      value <- fvParse (fdValue descr) (T.strip (vValue v))
      parseFields (set l value s) (Seq.viewl fs) sect
  | fdSkipIfMissing descr =
      parseFields s (Seq.viewl fs) sect
  | otherwise = Left ("Unable to find field " ++
                      show (normalizedText (fdName descr)))
parseFields s (FieldMb l descr Seq.:< fs) sect
  | Just v <- lkp (fdName descr) (isVals sect) = do
      value <- fvParse (fdValue descr) (T.strip (vValue v))
      parseFields (set l (Just value) s) (Seq.viewl fs) sect
  | otherwise =
      parseFields (set l Nothing s) (Seq.viewl fs) sect

-- | Serialize a value as an INI file according to a provided
-- 'IniSpec'.
emitIniFile :: s -> Spec s -> RawIni
emitIniFile s spec =
  RawIni $
    fmap (\ (Section name fs _) ->
             (name, toSection s (actualText name) fs)) spec

mkComments :: Seq Text -> Seq BlankLine
mkComments comments =
  fmap (\ ln -> CommentLine '#' (" " <> ln)) comments

toSection :: s -> Text -> Seq (Field s) -> IniSection
toSection s name fs = IniSection
  { isName = name
  , isVals = fmap toVal fs
  , isStartLine = 0
  , isEndLine   = 0
  , isComments  = Seq.empty
  } where mkIniValue val descr opt =
            ( fdName descr
            , IniValue
                { vLineNo = 0
                , vName   = actualText (fdName descr)
                , vValue  = " " <> val
                , vComments = mkComments (fdComment descr)
                , vCommentedOut = opt
                , vDelimiter = '='
                }
            )
          toVal (Field l descr)
            | Just dummy <- fdDummy descr =
                mkIniValue dummy descr False
            | otherwise =
                mkIniValue (fvEmit (fdValue descr) (get l s)) descr False
          toVal (FieldMb l descr)
            | Just dummy <- fdDummy descr =
                mkIniValue dummy descr True
            | Just v <- get l s =
                mkIniValue (fvEmit (fdValue descr) v) descr True
            | otherwise =
                mkIniValue "" descr True

-- | An 'UpdatePolicy' guides certain choices made when an 'Ini' file
-- is updated: for example, how to add comments to the generated
-- fields, or how to treat fields which are optional.
data UpdatePolicy = UpdatePolicy
  { updateAddOptionalFields      :: Bool
    -- ^ If 'True', then optional fields not included in the INI file
    -- will be included in the updated INI file. Defaults to 'False'.
  , updateIgnoreExtraneousFields :: Bool
    -- ^ If 'True', then fields in the INI file that have no
    -- corresponding description in the 'IniSpec' will be ignored; if
    -- 'False', then those fields will return an error value. Defaults
    -- to 'True'.
  , updateGeneratedCommentPolicy :: UpdateCommentPolicy
    -- ^ The policy for what to do to comments associated with
    -- modified fields during an update. Defaults to
    -- 'CommentPolicyNone'.
  } deriving (Eq, Show)

-- | A set of sensible 'UpdatePolicy' defaults which keep the diffs
-- between file versions minimal.
defaultUpdatePolicy :: UpdatePolicy
defaultUpdatePolicy = UpdatePolicy
  { updateAddOptionalFields = False
  , updateIgnoreExtraneousFields = True
  , updateGeneratedCommentPolicy = CommentPolicyNone
  }

-- | An 'UpdateCommentPolicy' describes what comments should accompany
-- a field added to or modified in an existing INI file when using
-- 'updateIni'.
data UpdateCommentPolicy
  = CommentPolicyNone
    -- ^ Do not add comments to new fields
  | CommentPolicyAddFieldComment
    -- ^ Add the same comment which appears in the 'IniSpec' value for
    -- the field we're adding or modifying.
  | CommentPolicyAddDefaultComment (Seq Text)
    -- ^ Add a common comment to all new fields added or modified
    -- by an 'updateIni' call.
    deriving (Eq, Show)

getComments :: FieldDescription s -> UpdateCommentPolicy -> (Seq BlankLine)
getComments _ CommentPolicyNone = Seq.empty
getComments f CommentPolicyAddFieldComment =
  mkComments (fdComment f)
getComments _ (CommentPolicyAddDefaultComment cs) =
  mkComments cs

-- | Given a value, an 'IniSpec', and a 'Text' form of an INI file,
-- parse 'Text' as INI and then selectively modify the file whenever
-- the provided value differs from the file. This is designed to help
-- applications update a user's configuration automatically while
-- retaining the structure and comments of a user's application,
-- ideally in a way which produces as few changes as possible to the
-- resulting file (so that, for example, the diff between the two
-- should be as small as possible.)
--
--  A field is considered to have "changed" if the parsed
--  representation of the field as extracted from the textual INI file
--  is not equal to the corresponding value in the provided
--  structure. Changed fields will retain their place in the overall
--  file, while newly added fields (for example, fields which have
--  been changed from a default value) will be added to the end of the
--  section in which they appear.
--doUpdateIni :: s -> s -> Spec s -> RawIni -> UpdatePolicy -> Either String (Ini s)
doUpdateIni :: s -> Ini s -> Either String (Ini s)
doUpdateIni s i@Ini { iniSpec = spec
                    , iniDef = def
                    , iniPol = pol
                    } = do -- spec (RawIni ini) pol = do
  let RawIni ini' = getRawIni i
  res <- updateSections s def ini' spec pol
  return $ i
    { iniCurr = s
    , iniLast = Just (RawIni res)
    }

updateSections
  :: s
  -> s
  -> Seq (NormalizedText, IniSection)
  -> Seq (Section s)
  -> UpdatePolicy
  -> Either String (Seq (NormalizedText, IniSection))
updateSections s def sections fields pol = do
  -- First, we process all the sections that actually appear in the
  -- INI file in order
  existingSections <- F.for sections $ \ (name, sec) -> do
    let err  = Left ("Unexpected top-level section: " ++ show name)
    Section _ spec _ <- maybe err Right
      (F.find (\ (Section n _ _) -> n == name) fields)
    newVals <- updateFields s (isVals sec) spec pol
    return (name, sec { isVals = newVals })
  -- And then
  let existingSectionNames = fmap fst existingSections
  newSections <- F.for fields $
    \ (Section nm spec _) ->
      if | nm `elem` existingSectionNames -> return mempty
         | otherwise ->
           let rs = emitNewFields s def spec pol
           in if Seq.null rs
                then return mempty
                else return $ Seq.singleton
                       ( nm
                       , IniSection (actualText nm) rs 0 0 mempty
                       )
  return (existingSections <> F.asum newSections)

-- We won't emit a section if everything in the section is also
-- missing
emitNewFields
  :: s -> s
  -> Seq (Field s)
  -> UpdatePolicy ->
  Seq (NormalizedText, IniValue)
emitNewFields s def fields pol = go (Seq.viewl fields) where
  go EmptyL = Seq.empty
  go (Field l d :< fs)
    -- If a field is not present but is also the same as the default,
    -- then we can safely omit it
    | get l s == get l def && not (updateAddOptionalFields pol) =
      go (Seq.viewl fs)
    -- otherwise, we should add it to the result
    | otherwise =
      let cs = getComments d (updateGeneratedCommentPolicy pol)
          new = ( fdName d
                , IniValue
                  { vLineNo       = 0
                  , vName         = actualText (fdName d)
                  , vValue        = " " <> fvEmit (fdValue d) (get l s)
                  , vComments     = cs
                  , vCommentedOut = False
                  , vDelimiter    = '='
                  }
                )
      in new <| go (Seq.viewl fs)
  go (FieldMb l d :< fs) =
    case get l s of
      Nothing -> go (Seq.viewl fs)
      Just v ->
        let cs = getComments d (updateGeneratedCommentPolicy pol)
            new = ( fdName d
                  , IniValue
                    { vLineNo       = 0
                    , vName         = actualText (fdName d)
                    , vValue        = fvEmit (fdValue d) v
                    , vComments     = cs
                    , vCommentedOut = False
                    , vDelimiter    = '='
                    }
                  )
        in new <| go (Seq.viewl fs)


updateFields :: s -> Seq (NormalizedText, IniValue) -> Seq (Field s)
                 -> UpdatePolicy -> Either String (Seq (NormalizedText, IniValue))
updateFields s values fields pol = go (Seq.viewl values) fields
  where go ((t, val) :< vs) fs =
          -- For each field, we need to fetch the description of the
          -- field in the spec
          case F.find (\ f -> fieldName f == t) fs of
            Just f@(Field l descr) ->
              -- if it does exist, then we need to find out whether
              -- the field has changed at all. We can do this with the
              -- provided lens, and check it against the INI file
              -- we've got. There's a minor complication: there's
              -- nothing that forces the user to provide the same INI
              -- file we originally parsed! One side-effect means that
              -- the parsed INI file might not actually have a valid
              -- field according to the field parser the user
              -- provides. In that case, we'll assume the field is
              -- outdated, and update it with the value in the
              -- provided structure.
              if Right (get l s) == fvParse (fdValue descr) (T.strip (vValue val))
                 -- if the value in the INI file parses the same as
                 -- the one in the structure we were passed, then it
                 -- doesn't need any updating, and we keep going,
                 -- removing the field from our list
                then ((t, val) <|) `fmap` go (Seq.viewl vs) (rmv t fs)
                 -- otherwise, we've got a new updated value! Let's
                 -- synthesize a new element, using our comment policy
                 -- to comment it accordingly. (This pattern is
                 -- partial, but we should never have a situation
                 -- where it returns Nothing, because we already know
                 -- that we've matched a Field!)
                else let Just nv = mkValue t f (vDelimiter val)
                     in ((t, nv) <|) `fmap` go (Seq.viewl vs) (rmv t fs)
              -- And we have to replicate the logic for the FieldMb
              -- case, because (as an existential) it doesn't really
              -- permit us usable abstractions here. See the previous
              -- comments for descriptions of the cases.
            Just f@(FieldMb l descr) ->
              let parsed = fvParse (fdValue descr) (T.strip (vValue val))
              in if Right (get l s) == fmap Just parsed
                  then ((t, val) <|) `fmap` go (Seq.viewl vs) (rmv t fs)
                   -- this is in the only case where the FieldMb case
                   -- differs: we might NOT have a value in the
                   -- structure. In that case, we remove the value
                   -- from the file, as well!
                  else case mkValue t f (vDelimiter val) of
                         Just nv -> ((t, nv) <|) `fmap` go (Seq.viewl vs) (rmv t fs)
                         Nothing -> go (Seq.viewl vs) (rmv t fs)
            -- Finally, if we can't find any description of the field,
            -- then we might skip it or throw an error, depending on
            -- the policy the user wants.
            Nothing
              | updateIgnoreExtraneousFields pol ->
                ((t, val) <|) `fmap` go (Seq.viewl vs) fs
              | otherwise -> Left ("Unexpected field: " ++ show t)
        -- Once we've gone through all the fields in the file, we need
        -- to see if there's anything left over that should be in the
        -- file. We might want to include dummy values for things that
        -- were left out, but if we have any non-optional fields left
        -- over, then we definitely need to include them.
        go EmptyL fs = return (finish (Seq.viewl fs))
        finish (f@(Field {}) :< fs)
          | updateAddOptionalFields pol
          , Just val <- mkValue (fieldName f) f '=' =
            (fieldName f, val) <| finish (Seq.viewl fs)
          | otherwise = finish (Seq.viewl fs)
        finish (f@(FieldMb _ descr) :< fs)
          | not (fdSkipIfMissing descr)
          , Just val <- mkValue (fieldName f) f '=' =
            (fieldName f, val) <| finish (Seq.viewl fs)
          | updateAddOptionalFields pol
          , Just val <- mkValue (fieldName f) f '=' =
            (fieldName f, val) <| finish (Seq.viewl fs)
          | otherwise = finish (Seq.viewl fs)
        -- If there's nothing left, then we can return a final value!
        finish EmptyL = Seq.empty
        mkValue t fld delim =
          let comments = case updateGeneratedCommentPolicy pol of
                CommentPolicyNone -> Seq.empty
                CommentPolicyAddFieldComment ->
                  mkComments (fieldComment fld)
                CommentPolicyAddDefaultComment cs ->
                  mkComments cs
              val = IniValue
                      { vLineNo       = 0
                      , vName         = actualText t
                      , vValue        = ""
                      , vComments     = comments
                      , vCommentedOut = False
                      , vDelimiter    = delim
                      }
          in case fld of
               Field l descr ->
                 Just (val { vValue = " " <> fvEmit (fdValue descr) (get l s) })
               FieldMb l descr ->
                 case get l s of
                   Just v  -> Just (val { vValue = " " <> fvEmit (fdValue descr) v })
                   Nothing -> Nothing


-- $using
-- Functions for parsing, serializing, and updating INI files.

-- $types
-- Types which represent declarative specifications for INI
-- file structure.

-- $sections
-- Declaring sections of an INI file specification

-- $fields
-- Declaring individual fields of an INI file specification.

-- $fieldvalues
-- Values of type 'FieldValue' represent both a parser and a
-- serializer for a value of a given type. It's possible to manually
-- create 'FieldValue' descriptions, but for simple configurations,
-- but for the sake of convenience, several commonly-needed
-- varieties of 'FieldValue' are defined here.

-- $misc
-- These values and types are exported for compatibility.
