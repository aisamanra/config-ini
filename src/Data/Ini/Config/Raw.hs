module Data.Ini.Config.Raw
( Ini(..)
, IniSection(..)
, IniValue(..)
, parseIni
) where

import           Control.Monad (void)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text

-- | An 'Ini' value is a mapping from section names to
--   'IniSection' values.
newtype Ini
  = Ini { fromIni :: HashMap Text IniSection }
    deriving (Eq, Show)

-- | An 'IniSection' consists of a name, a mapping of key-value pairs,
--   and metadata about where the section starts and ends in the file.
data IniSection = IniSection
  { isName      :: Text
  , isVals      :: HashMap Text IniValue
  , isStartLine :: Int
  , isEndLine   :: Int
  } deriving (Eq, Show)

-- | An 'IniValue' represents a key-value mapping, and also stores the
--   line number where it appears.
data IniValue = IniValue
  { vLineNo :: Int
  , vName   :: Text
  , vValue  :: Text
  } deriving (Eq, Show)

-- | Parse a 'Text' value into an 'Ini' value.
parseIni :: Text -> Either String Ini
parseIni t = case runParser pIni "ini file" t of
  Left err -> Left (parseErrorPretty err)
  Right v  -> Right v

pIni :: Parser Ini
pIni = sBlanks *> (go `fmap` (many (pSection <?> "section") <* eof))
  where go vs = Ini $ HM.fromList [ (T.toLower (isName v), v)
                                  | v <- vs
                                  ]

sBlanks :: Parser ()
sBlanks = skipMany (void eol <|> sComment)

sComment :: Parser ()
sComment = do
  void (oneOf ";#")
  void (manyTill anyChar eol)

pSection :: Parser IniSection
pSection = do
  start <- getCurrentLine
  void (char '[')
  name <- T.pack `fmap` some (noneOf "[]")
  void (char ']')
  sBlanks
  vals <- many (pPair <?> "key-value pair")
  end <- getCurrentLine
  sBlanks
  return IniSection
    { isName      = T.strip name
    , isVals      = HM.fromList [ (vName v, v) | v <- vals ]
    , isStartLine = start
    , isEndLine   = end
    }

pPair :: Parser IniValue
pPair = do
  pos <- getCurrentLine
  key <- T.pack `fmap` some (noneOf "[]=:")
  void (oneOf ":=")
  val <- T.pack `fmap` manyTill anyChar eol
  sBlanks
  return IniValue
    { vLineNo = pos
    , vName   = T.strip key
    , vValue  = T.strip val
    }

getCurrentLine :: Parser Int
getCurrentLine = (fromIntegral . unPos . sourceLine) `fmap` getPosition
