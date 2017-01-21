module Data.Ini.Config.Raw
( Ini(..)
, IniSection(..)
, IniValue(..)
, BlankLine(..)
, parseIni
, printIni
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
import           Text.Megaparsec
import           Text.Megaparsec.Text

-- | An 'Ini' value is a mapping from section names to
--   'IniSection' values.
newtype Ini = Ini
  { fromIni :: Seq (Text, IniSection)
  } deriving (Eq, Show)

-- | An 'IniSection' consists of a name, a mapping of key-value pairs,
--   and metadata about where the section starts and ends in the file.
data IniSection = IniSection
  { isName      :: Text
  , isVals      :: Seq (Text, IniValue)
  , isStartLine :: Int
  , isEndLine   :: Int
  , isComments  :: Seq BlankLine
  } deriving (Eq, Show)

-- | An 'IniValue' represents a key-value mapping, and also stores the
--   line number where it appears.
data IniValue = IniValue
  { vLineNo       :: Int
  , vName         :: Text
  , vValue        :: Text
  , vComments     :: Seq BlankLine
  , vCommentedOut :: Bool
    -- ^ Right now, this will never show up in a parsed INI file, but
    --   it's used when emitting a default INI file: it causes the
    --   key-value line to include a leading comment as well.
  } deriving (Eq, Show)

-- | We want to keep track of the whitespace/comments in between KV
--   lines, so this allows us to track those lines in a reproducible
--   way.
data BlankLine
  = CommentLine Char Text
  | BlankLine
    deriving (Eq, Show)

-- | Parse a 'Text' value into an 'Ini' value.
parseIni :: Text -> Either String Ini
parseIni t = case runParser pIni "ini file" t of
  Left err -> Left (parseErrorPretty err)
  Right v  -> Right v

pIni :: Parser Ini
pIni = do
  leading <- sBlanks
  pSections leading Seq.empty

sBlanks :: Parser (Seq BlankLine)
sBlanks = Seq.fromList <$> many ((BlankLine <$ void eol) <|> sComment)

sComment :: Parser BlankLine
sComment = do
  c <- oneOf ";#"
  txt <- T.pack `fmap` manyTill anyChar eol
  return (CommentLine c txt)

pSections :: Seq BlankLine -> Seq (Text, IniSection) -> Parser Ini
pSections leading prevs =
  pSection leading prevs <|> (Ini prevs <$ void eof)

pSection :: Seq BlankLine -> Seq (Text, IniSection) -> Parser Ini
pSection leading prevs = do
  start <- getCurrentLine
  void (char '[')
  name <- T.pack `fmap` some (noneOf "[]")
  void (char ']')
  comments <- sBlanks
  pPairs (T.strip name) start leading prevs comments Seq.empty

pPairs :: Text
       -> Int
       -> Seq BlankLine
       -> Seq (Text, IniSection)
       -> Seq BlankLine
       -> Seq (Text, IniValue)
       -> Parser Ini
pPairs name start leading prevs comments pairs = newPair <|> finishedSection
  where
    newPair = do
      pair <- pPair comments
      rs <- sBlanks
      pPairs name start leading prevs rs (pairs Seq.|> (vName pair, pair))
    finishedSection = do
      end <- getCurrentLine
      let newSection = IniSection
            { isName      = name
            , isVals      = pairs
            , isStartLine = start
            , isEndLine   = end
            , isComments  = leading
            }
      pSections comments (prevs Seq.|> (T.toLower name, newSection))

pPair :: Seq BlankLine -> Parser IniValue
pPair leading = do
  pos <- getCurrentLine
  key <- T.pack `fmap` some (noneOf "[]=:")
  void (oneOf ":=")
  val <- T.pack `fmap` manyTill anyChar eol
  return IniValue
    { vLineNo       = pos
    , vName         = T.strip key
    , vValue        = T.strip val
    , vComments     = leading
    , vCommentedOut = False
    }

getCurrentLine :: Parser Int
getCurrentLine = (fromIntegral . unPos . sourceLine) `fmap` getPosition


printIni :: Ini -> Text
printIni = LazyText.toStrict . Builder.toLazyText . F.foldMap build . fromIni
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
      Builder.fromString " = " <>
      Builder.fromText (vValue val) <>
      Builder.singleton '\n'
