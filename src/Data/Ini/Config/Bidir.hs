{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ini.Config.Bidir
(
-- $main
-- * Parsing, Serializing, and Updating Files
  parseIniFile
, emitIniFile
, UpdatePolicy(..)
, UpdateCommentPolicy(..)
, defaultUpdatePolicy
, updateIniFile
-- * Bidirectional Parser Types
, IniSpec
, SectionSpec
-- * Section-Level Parsing
, section
-- * Field-Level Parsing
, (.=)
, (.=?)
, field
, flag
, comment
, defaultValue
, placeholderValue
, skipIfMissing
-- * FieldValues
, FieldValue(..)
, text
, string
, number
, bool
, readable
, listWithSeparator
-- * Miscellaneous Helpers
, (&)
, Lens
) where

import           Control.Monad.Trans.State.Strict (State, runState, modify)
import qualified Data.Foldable as F
#if __GLASGOW_HASKELL__ >= 710
import           Data.Function ((&))
#endif
import           Data.Monoid ((<>))
import           Data.Sequence ((<|), Seq, ViewL(..))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Traversable as F
import           Data.Typeable (Typeable, Proxy(..), typeRep)
import           GHC.Exts (IsList(..))
import           Text.Read (readMaybe)

import           Data.Ini.Config.Raw

-- | This is a "lens"-compatible type alias
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lkp :: Text -> Seq (Text, a) -> Maybe a
lkp t = go . Seq.viewl
  where go ((t', x) Seq.:< rs)
          | T.toLower t == T.toLower t' = Just x
          | otherwise = go (Seq.viewl rs)
        go Seq.EmptyL = Nothing

rmv :: Text -> Seq (Field s) -> Seq (Field s)
rmv n = Seq.filter (\ f -> T.toLower (fieldName f) /= T.toLower n)

fieldName :: Field s -> Text
fieldName (Field _ FieldDescription { fdName = n }) = n
fieldName (FieldMb _ FieldDescription { fdName = n }) = n

fieldComment :: Field s -> Seq Text
fieldComment (Field _ FieldDescription { fdComment = n }) = n
fieldComment (FieldMb _ FieldDescription { fdComment = n }) = n

#if __GLASGOW_HASKELL__ < 710
{- | '&' is a reverse application operator. This provides notational
     convenience. Its precedence is one higher than that of the
     forward application operator '$', which allows '&' to be nested
     in '$'. -}
(&) :: a -> (a -> b) -> b
a & f = f a
infixl 1 &
#endif

-- | A value of type "FieldValue" packages up a parser and emitter
--   function into a single value. These are used for bidirectional
--   parsing and emitting of the value of a field.
data FieldValue a = FieldValue
  { fvParse :: Text -> Either String a
    -- ^ The function to use when parsing the value of a field; if
    --   the parser fails, then the string will be shown as an error
    --   message to the user.
  , fvEmit  :: a -> Text
    -- ^ The serializer to use when serializing a value into an INI file.
  }

-- This is actually being used as a writer monad, but using a state
-- monad lets us avoid the space leaks. Not that those are likely to
-- be a problem in this application, but it's not like it cost us
-- none.
type BidirM s a = State (Seq s) a

runBidirM :: BidirM s a -> Seq s
runBidirM = snd . flip runState Seq.empty

-- | An 'IniSpec' value represents the structure of an entire
-- INI-format file in a declarative way. The @s@ parameter represents
-- the type of a Haskell structure which is being serialized to or
-- from.
newtype IniSpec s a = IniSpec (BidirM (Text, Seq (Field s)) a)
  deriving (Functor, Applicative, Monad)

-- | A 'SectionSpec' value represents the structure of a single
-- section of an INI-format file in a declarative way. The @s@
-- parameter represents the type of a Haskell structure which is being
-- serialized to or from.
newtype SectionSpec s a = SectionSpec (BidirM (Field s) a)
  deriving (Functor, Applicative, Monad)

-- |
section :: Text -> SectionSpec s () -> IniSpec s ()
section name (SectionSpec mote) = IniSpec $ do
  let fields = runBidirM mote
  modify (Seq.|> (name, fields))

data Field s
  = forall a. Eq a => Field (Lens s s a a) (FieldDescription a)
  | forall a. Eq a => FieldMb (Lens s s (Maybe a) (Maybe a)) (FieldDescription a)

data FieldDescription t = FieldDescription
  { fdName          :: Text
  , fdValue         :: FieldValue t
  , fdDefault       :: Maybe t
  , fdComment       :: Seq Text
  , fdDummy         :: Maybe Text
  , fdSkipIfMissing :: Bool
  }

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

{- |
Associate a multiline comment with a "FieldDescription". When
serializing a field that has a comment associated, the comment will
appear before the field.
-}
comment :: [Text] -> FieldDescription t -> FieldDescription t
comment cmt fd = fd { fdComment = Seq.fromList cmt }

{- |
Choose a default value to be used in case of a missing value. This will
only be used in the case of non-optional fields.
-}
defaultValue :: t -> FieldDescription t -> FieldDescription t
defaultValue def fd = fd { fdDefault = Just def }

-- | Choose a placeholder value to be displayed for optional fields.
--   This is used when serializing an optional Ini field: the
--   field will appear commented out in the output using the
--   placeholder text as a value, so a spec that includes
--
--   @
--   myLens .=? field "x" & placeholderValue "<val>"
--   @
--
--   will serialize into an INI file that contains the line
--
--   @
--   # x = <val>
--   @
--
--   A placeholder value will only appear in the serialized output
--   if the field is optional, but will be preferred over serializing
--   a "defaultValue". If a "placeholderValue" is not
placeholderValue :: Text -> FieldDescription t -> FieldDescription t
placeholderValue t fd = fd { fdDummy = Just t }

-- | If the field is not found in parsing, simply skip instead of
--   raising an error or setting anything.
skipIfMissing :: FieldDescription t -> FieldDescription t
skipIfMissing fd = fd { fdSkipIfMissing = True }

infixr 0 .=
infixr 0 .=?

-- | Create a description of a field by a combination of the name of
--   the field and a "FieldValue" describing how to parse and emit
--   the
field :: Text -> FieldValue a -> FieldDescription a
field name value = FieldDescription
  { fdName          = name
  , fdValue         = value
  , fdDefault       = Nothing
  , fdComment       = Seq.empty
  , fdDummy         = Nothing
  , fdSkipIfMissing = False
  }

flag :: Text -> FieldDescription Bool
flag name = field name bool

-- | A "FieldValue" implementation for parsing and reading
--   values according to the logic of the "Read" and "Show"
--   instances for that type, providing a convenient
--   human-readable error message if the parsing step fails.
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

-- | A "FieldValue" implementation for parsing and reading numeric
--   values according to the logic of the "Read" and "Show"
--   instances for that type.
number :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number = readable

-- |
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

listWithSeparator :: IsList l => Text -> FieldValue (Item l) -> FieldValue l
listWithSeparator sep fv = FieldValue
  { fvParse = fmap fromList . mapM (fvParse fv . T.strip) . T.splitOn sep
  , fvEmit  = T.intercalate sep . map (fvEmit fv) . toList
  }

-- | Provided an initial value and an 'IniSpec' describing the
-- structure of an INI file, parse a 'Text' value as an INI file,
-- update the initial value corresponding to the fields in the INI
-- file, and then return the modified value.
parseIniFile :: s -> IniSpec s () -> Text -> Either String s
parseIniFile def (IniSpec mote) t =
  let spec = runBidirM mote
  in case parseIni t of
    Left err        -> Left err
    Right (Ini ini) -> runSpec def (Seq.viewl spec) ini

-- Are you reading this source code? It's not even that gross
-- yet. Just you wait. This is just the regular part. 'runSpec' is
-- easy: we walk the spec, and for each section, find the
-- corresponding section in the INI file and call runFields.
runSpec :: s -> Seq.ViewL (Text, Seq (Field s)) -> Seq (Text, IniSection)
        -> Either String s
runSpec s Seq.EmptyL _ = Right s
runSpec s ((name, fs) Seq.:< rest) ini
  | Just v <- lkp (T.toLower name) ini = do
      s' <- runFields s (Seq.viewl fs) v
      runSpec s' (Seq.viewl rest) ini
  | otherwise = Left ("Unable to find section " ++ show name)

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

-- Now that we've got 'set', we can walk the field descriptions and
-- find them. There's some fiddly logic, but the high-level idea is
-- that we try to look up a field, and if it exists, parse it using
-- the provided parser and use the provided lens to add it to the
-- value. We have to decide what to do if it's not there, which
-- depends on lens metadata and whether it's an optional field or not.
runFields :: s -> Seq.ViewL (Field s) -> IniSection -> Either String s
runFields s Seq.EmptyL _ = Right s
runFields s (Field l descr Seq.:< fs) sect
  | Just v <- lkp (fdName descr) (isVals sect) = do
      value <- fvParse (fdValue descr) (T.strip (vValue v))
      runFields (set l value s) (Seq.viewl fs) sect
  | Just def <- fdDefault descr =
      runFields (set l def s) (Seq.viewl fs) sect
  | otherwise = Left ("Unable to find field " ++ show (fdName descr))
runFields s (FieldMb l descr Seq.:< fs) sect
  | Just v <- lkp (fdName descr) (isVals sect) = do
      value <- fvParse (fdValue descr) (T.strip (vValue v))
      runFields (set l (Just value) s) (Seq.viewl fs) sect
  | otherwise =
      runFields (set l Nothing s) (Seq.viewl fs) sect

-- | Serialize a value as an INI file according to a provided
-- 'IniSpec'.
emitIniFile :: s -> IniSpec s () -> Text
emitIniFile s (IniSpec mote) =
  let spec = runBidirM mote in
  printIni $ Ini $ fmap (\ (name, fs) -> (name, toSection s name fs)) spec

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
  } where mkIniValue val descr optional =
            ( fdName descr
            , IniValue
                { vLineNo = 0
                , vName   = fdName descr
                , vValue  = val
                , vComments = BlankLine <| mkComments (fdComment descr)
                , vCommentedOut = optional
                , vDelimiter = '='
                }
            )
          toVal (Field l descr) =
            mkIniValue (fvEmit (fdValue descr) (get l s)) descr False
          toVal (FieldMb l descr) =
            case get l s of
              Nothing
                | Just d <- fdDefault descr ->
                    mkIniValue (fvEmit (fdValue descr) d) descr True
                | otherwise ->
                    mkIniValue "" descr True
              Just v ->
                mkIniValue (fvEmit (fdValue descr) v) descr True

-- | An 'UpdatePolicy' describes how to
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
-- 'updateIniFile'.
data UpdateCommentPolicy
  = CommentPolicyNone
    -- ^ Do not add comments to new fields
  | CommentPolicyAddFieldComment
    -- ^ Add the same comment which appears in the 'IniSpec' value for
    -- the field we're adding or modifying.
  | CommentPolicyAddDefaultComment (Seq Text)
    -- ^ Add a consistent comment to all new fields added or modified
    -- by an 'updateIniFile' call.
    deriving (Eq, Show)

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
updateIniFile :: s -> IniSpec s () -> Text -> UpdatePolicy -> Either String Text
updateIniFile s (IniSpec mote) t pol =
  let spec = runBidirM mote
  in case parseIni t of
    Left err -> Left ("Error parsing existing INI file: " ++ err)
    Right (Ini ini) -> do
      ini' <- updateIniSections s ini spec pol
      return (printIni (Ini ini'))

updateIniSections :: s -> Seq (Text, IniSection)
                  -> Seq (Text, Seq (Field s))
                  -> UpdatePolicy
                  -> Either String (Seq (Text, IniSection))
updateIniSections s sections fields pol =
  F.for sections $ \ (name, sec) -> do
    let err  = (Left ("Unexpected top-level section: " ++ show name))
    spec <- maybe err Right (lkp name fields)
    newVals <- updateIniSection s (isVals sec) spec pol
    return (name, sec { isVals = newVals })

updateIniSection :: s -> Seq (Text, IniValue) -> Seq (Field s)
                 -> UpdatePolicy -> Either String (Seq (Text, IniValue))
updateIniSection s values fields pol = go (Seq.viewl values) fields
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
        finish (f@(Field l descr) :< fs)
          | or [ updateAddOptionalFields pol
               , fdDefault descr /= Just (get l s)
               ]
          , Just val <- mkValue (fieldName f) f '=' =
            (fieldName f, val) <| finish (Seq.viewl fs)
          | otherwise = finish (Seq.viewl fs)
        finish (f@(FieldMb _ descr) :< fs)
          | not (fdSkipIfMissing descr) && fdDefault descr == Nothing
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
                      , vName         = t <> " "
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


-- DELETE ME LATER

lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens gt st f a = (`st` a) `fmap` f (gt a)

_1 :: Lens (a, b) (a, b) a a
_1 = lens fst (\ a (_, b) -> (a, b))

_2 :: Lens (a, b) (a, b) b b
_2 = lens snd (\ b (a, _) -> (a, b))



{- $main This module is an alternate API used for parsing INI files.
Unlike the standard API, it is bidirectional: the same declarative
structure can be also used to emit an INI file, or even to produce an
updated INI file with minimal modification to the textual file
provided.

This module makes some extra assumptions about your configuration type
and the way you interact with it: in particular, it assumes that you
have lenses for all the fields you're parsing, and that you have some
kind of sensible default value of that configuration. Instead of
providing combinators which can extract and parse a field of an INI
file into a value, the bidirectional API allows you to declaratively
map lenses into your structure to descriptions of corresponding fields
in INI files.

Consider the following example INI file:

> [NETWORK]
> host = example.com
> port = 7878
>
> [LOCAL]
> user = terry

We'd like to parse this INI file into a @Config@ type which we've
defined like this, using "lens" or a similar library to provide
lenses:

> data Config = Config
>   { _cfHost :: String
>   , _cfPort :: Int
>   , _cfUser :: Maybe Text
>   } deriving (Eq, Show)
>
> ''makeLenses Config

We can now define a basic specification of the type @IniSpec Config
()@ by using the provided operations to declare our top-level
sections, and then within those sections associate fields with lenses
into our @Config@ structure.

> configSpec :: IniSpec Config ()
> configSpec = do
>   section "NETWORK" $ do
>     cfHost .= field "host" string
>     cfPost .= field "port" number
>   section "LOCAL" $ do
>     cfUser .=? field "user" text

The '.=' operator associates a field with a lens directly, and the
'.=?' operator associates a field with a lens to a 'Maybe' value,
setting that value to 'Nothing' if the field does not appear in the
configuration. Each 'field' invocation must include the name of the
field and a representation of the type of that field: 'string',
'number', and 'text' in the above snippet are all values of type
'FieldValue', which bundle together a parser and serializer so that
they can be used bidirectionally.

We can also provide extra metadata about a field, allowing it to be
skipped in parsing, or to provide an explicit default value, or to
include an explanatory comment for that value to be used when we
serialize an INI file. These are conventionally applied to the field
using the '&' operator:

> configSpec :: IniSpec Config ()
> configSpec = do
>   section "NETWORK" $ do
>     cfHost .= field "host" string
>                 & comment ["The desired hostname (optional)"]
>                 & skipIfMissing
>     cfPost .= field "port" number
>                 & comment ["The port number"]
>                 & defaultValue 9999
>   section "LOCAL" $ do
>     cfUser .=? field "user" text

In order to parse an INI file, we need to provide a default value of
our underlying @Config@ type on which we can perform our 'Lens'-based
updates. Parsing will then walk the specification and update each
field in the default value to the field provided in the INI file. We
can also use a value of our @Config@ type and serialize it directly,
which is useful for generating a default configuration: this will
include the comments we've provided and (optionally) commented-out
key-value pairs representing default values. Finally, we can /update/
a configuration file, reflecting changes to a value back to an
existing INI file in a way that preserves incidental structure like
spacing and comments.

-}
