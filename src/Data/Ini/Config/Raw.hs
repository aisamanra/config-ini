{-|
Module     : Data.Ini.Config.Raw
Copyright  : (c) Getty Ritter, 2017
License    : BSD
Maintainer : Getty Ritter <config-ini@infinitenegativeutility.com>
Stability  : experimental

__Warning!__ This module is subject to change in the future, and therefore should
not be relied upon to have a consistent API.

-}
module Data.Ini.Config.Raw
( -- * INI types
  RawIni(..)
, IniSection(..)
, IniValue(..)
, BlankLine(..)
, NormalizedText(..)
, normalize
  -- * serializing and deserializing
, parseRawIni
, printRawIni
  -- * inspection
, lookupInSection
, lookupSection
, lookupValue
) where

import           Control.Monad (void)
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | The 'NormalizedText' type is an abstract representation of text
-- which has had leading and trailing whitespace removed and been
-- normalized to lower-case, but from which we can still extract the
-- original, non-normalized version. This acts like the normalized
-- text for the purposes of 'Eq' and 'Ord' operations, so
--
-- @
--   'normalize' "  x  " == 'normalize' \"X\"
-- @
--
-- This type is used to store section and key names in the
data NormalizedText = NormalizedText
  { actualText     :: Text
  , normalizedText :: Text
  } deriving (Show)

-- | The constructor function to build a 'NormalizedText' value. You
-- probably shouldn't be using this module directly, but if for some
-- reason you are using it, then you should be using this function to
-- create 'NormalizedText' values.
normalize :: Text -> NormalizedText
normalize t = NormalizedText t (T.toLower (T.strip t))

instance Eq NormalizedText where
  NormalizedText _ x == NormalizedText _ y =
    x == y

instance Ord NormalizedText where
  NormalizedText _ x `compare` NormalizedText _ y =
    x `compare` y

-- | An 'Ini' value is a mapping from section names to
--   'IniSection' values. The section names in this mapping are
--   normalized to lower-case and stripped of whitespace. This
--   sequence retains the ordering of the original source file.
newtype RawIni = RawIni
  { fromRawIni :: Seq (NormalizedText, IniSection)
  } deriving (Eq, Show)

-- | An 'IniSection' consists of a name, a mapping of key-value pairs,
--   and metadata about where the section starts and ends in the
--   file. The section names found in 'isName' are __not__ normalized
--   to lower-case or stripped of whitespace, and thus should appear
--   exactly as they appear in the original source file.
data IniSection = IniSection
  { isName      :: Text
                   -- ^ The name of the section, as it appears in the
                   -- original INI source
  , isVals      :: Seq (NormalizedText, IniValue)
                   -- ^ The key-value mapping within that section. Key
                   -- names here are normalized to lower-case and
                   -- stripped of whitespace. This sequence retains
                   -- the ordering of the original source file.
  , isStartLine :: Int
                   -- ^ The line on which the section begins. This
                   -- field is ignored when serializing, and is only
                   -- used for error messages produced when parsing
                   -- and deserializing an INI structure.
  , isEndLine   :: Int
                   -- ^ The line on which the section ends. This field
                   -- is ignored when serializing, and is only used
                   -- for error messages produced when parsing and
                   -- deserializing an INI structure.
  , isComments  :: Seq BlankLine
                   -- ^ The blank lines and comments that appear prior
                   -- to the section head declaration, retained for
                   -- pretty-printing identical INI files.
  } deriving (Eq, Show)

-- | An 'IniValue' represents a key-value mapping, and also stores the
--   line number where it appears. The key names and values found in
--   'vName' and 'vValue' respectively are _not_ normalized to
--   lower-case or stripped of whitespace, and thus should appear
--   exactly as they appear in the original source file.
data IniValue = IniValue
  { vLineNo       :: Int
                     -- ^ The line on which the key/value mapping
                     -- appears. This field is ignored when
                     -- serializing, and is only used for error
                     -- messages produced when parsing and
                     -- deserializing an INI structure.
  , vName         :: Text
                     -- ^ The name of the key, as it appears in the INI source.
  , vValue        :: Text
                     -- ^ The value of the key
  , vComments     :: Seq BlankLine
  , vCommentedOut :: Bool
    -- ^ Right now, this will never show up in a parsed INI file, but
    --   it's used when emitting a default INI file: it causes the
    --   key-value line to include a leading comment as well.
  , vDelimiter    :: Char
  } deriving (Eq, Show)

-- | We want to keep track of the whitespace/comments in between KV
--   lines, so this allows us to track those lines in a reproducible
--   way.
data BlankLine
  = CommentLine Char Text
  | BlankLine
    deriving (Eq, Show)

-- | Parse a 'Text' value into an 'Ini' value, retaining a maximal
-- amount of structure as needed to reconstruct the original INI file.
parseRawIni :: Text -> Either String RawIni
parseRawIni t = case runParser pIni "ini file" t of
  Left err -> Left (errorBundlePretty err)
  Right v  -> Right v

pIni :: Parser RawIni
pIni = do
  leading <- sBlanks
  pSections leading Seq.empty

sBlanks :: Parser (Seq BlankLine)
sBlanks = Seq.fromList <$> many ((BlankLine <$ void eol) <|> sComment)

sComment :: Parser BlankLine
sComment = do
  c <- oneOf ";#"
  txt <- T.pack `fmap` manyTill anySingle eol
  return (CommentLine c txt)

pSections :: Seq BlankLine -> Seq (NormalizedText, IniSection) -> Parser RawIni
pSections leading prevs =
  pSection leading prevs <|> (RawIni prevs <$ void eof)

pSection :: Seq BlankLine -> Seq (NormalizedText, IniSection) -> Parser RawIni
pSection leading prevs = do
  start <- getCurrentLine
  void (char '[')
  name <- T.pack `fmap` some (noneOf "[]")
  void (char ']')
  void eol
  comments <- sBlanks
  pPairs (T.strip name) start leading prevs comments Seq.empty

pPairs :: Text
       -> Int
       -> Seq BlankLine
       -> Seq (NormalizedText, IniSection)
       -> Seq BlankLine
       -> Seq (NormalizedText, IniValue)
       -> Parser RawIni
pPairs name start leading prevs comments pairs = newPair <|> finishedSection
  where
    newPair = do
      (n, pair) <- pPair comments
      rs <- sBlanks
      pPairs name start leading prevs rs (pairs Seq.|> (n, pair))
    finishedSection = do
      end <- getCurrentLine
      let newSection = IniSection
            { isName      = name
            , isVals      = pairs
            , isStartLine = start
            , isEndLine   = end
            , isComments  = leading
            }
      pSections comments (prevs Seq.|> (normalize name, newSection))

pPair :: Seq BlankLine -> Parser (NormalizedText, IniValue)
pPair leading = do
  pos <- getCurrentLine
  key <- T.pack `fmap` some (noneOf "[]=:")
  delim <- oneOf ":="
  val <- T.pack `fmap` manyTill anySingle eol
  return ( normalize key
         , IniValue
             { vLineNo       = pos
             , vName         = key
             , vValue        = val
             , vComments     = leading
             , vCommentedOut = False
             , vDelimiter    = delim
             } )

getCurrentLine :: Parser Int
getCurrentLine = (fromIntegral . unPos . sourceLine) `fmap` getSourcePos


-- | Serialize an INI file to text, complete with any comments which
-- appear in the INI structure, and retaining the aesthetic details
-- which are present in the INI file.
printRawIni :: RawIni -> Text
printRawIni = LazyText.toStrict . Builder.toLazyText . F.foldMap build . fromRawIni
  where
    build (_, ini) =
      F.foldMap buildComment (isComments ini) <>
      Builder.singleton '[' <>
      Builder.fromText (isName ini) <>
      Builder.fromString "]\n" <>
      F.foldMap buildKV (isVals ini)
    buildComment BlankLine = Builder.singleton '\n'
    buildComment (CommentLine c txt) =
      Builder.singleton c <> Builder.fromText txt <> Builder.singleton '\n'
    buildKV (_, val) =
      F.foldMap buildComment (vComments val) <>
      (if vCommentedOut val then Builder.fromString "# " else mempty) <>
      Builder.fromText (vName val) <>
      Builder.singleton (vDelimiter val) <>
      Builder.fromText (vValue val) <>
      Builder.singleton '\n'

-- | Look up an Ini value by section name and key. Returns the sequence
-- of matches.
lookupInSection :: Text
                -- ^ The section name. Will be normalized prior to
                -- comparison.
                -> Text
                -- ^ The key. Will be normalized prior to comparison.
                -> RawIni
                -- ^ The Ini to search.
                -> Seq.Seq Text
lookupInSection sec opt ini =
    vValue <$> (F.asum (lookupValue opt <$> lookupSection sec ini))

-- | Look up an Ini section by name. Returns a sequence of all matching
-- section records.
lookupSection :: Text
              -- ^ The section name. Will be normalized prior to
              -- comparison.
              -> RawIni
              -- ^ The Ini to search.
              -> Seq.Seq IniSection
lookupSection name ini =
    snd <$> (Seq.filter ((== normalize name) . fst) $ fromRawIni ini)

-- | Look up an Ini key's value in a given section by the key. Returns
-- the sequence of matches.
lookupValue :: Text
            -- ^ The key. Will be normalized prior to comparison.
            -> IniSection
            -- ^ The section to search.
            -> Seq.Seq IniValue
lookupValue name section =
    snd <$> Seq.filter ((== normalize name) . fst) (isVals section)
