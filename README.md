# `config-ini`

[![Hackage](https://img.shields.io/hackage/v/config-ini.svg)](https://hackage.haskell.org/package/config-ini)

The `config-ini` library is a Haskell library for doing elementary INI file parsing in a quick and painless way.

## Basic Usage

The `config-ini` library exports some simple monadic functions to make parsing INI-like configuration easier. INI files have a two-level structure: the top-level named chunks of configuration, and the individual key-value pairs contained within those chunks. For example, the following INI file has two sections, `NETWORK` and `LOCAL`, and each section contains its own key-value pairs separated by either `=` or `:`. Comments, which begin with `#` or `;`, are ignored:

~~~.ini
[NETWORK]
host = example.com
port = 7878

# here is a comment
[LOCAL]
user = terry
~~~

The combinators provided here are designed to write quick and idiomatic parsers for basic INI files. Sections are parsed by `IniParser` computations, like `section` and its variations, while the fields within sections are parsed by `SectionParser` computations, like `field` and its variations. If we want to parse an INI file like the one above, treating the entire `LOCAL` section as optional, we can write it like this:

~~~.haskell
data Config = Config
  { cfNetwork :: NetworkConfig
  , cfLocal :: Maybe LocalConfig
  } deriving (Eq, Show)

data NetworkConfig = NetworkConfig
  { netHost :: String
  , netPort :: Int
  } deriving (Eq, Show)

data LocalConfig = LocalConfig
  { localUser :: Text
  } deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
  netCf <- section "NETWORK" $ do
    host <- fieldOf "host" string
    port <- fieldOf "port" number
    return NetworkConfig { netHost = host, netPort = port }
  locCf <- sectionMb "LOCAL" $
    LocalConfig <$> field "user"
  return Config { cfNetwork = netCf, cfLocal = locCf }
~~~

We can run our computation with `parseIniFile`, which, when run on our example file above, would produce the following:

~~~.haskell
>>> parseIniFile example configParser
Right (Config {cfNetwork = NetworkConfig {netHost = "example.com", netPort = 7878}, cfLocal = Just (LocalConfig {localUser = "terry"})})
~~~

## Combinators and Conventions

There are several variations on the same basic functionality that appear in `config-ini`. All functions that start with `section` are for parsing section-level chunks of an INI file, while all functions that start with `field` are for parsing key-value pairs within a section. Because it's reasonably common, there are also special `fieldFlag` functions which return `Bool` values, parsed in a relatively loose way.

All functions which end in `Mb` return a `Maybe` value, returning `Nothing` if the section or key was not found. All functions which end in `Def` take an additional default value, returning it if the section or key was not found. All functions which contain `Of` take a function of the type `Text -> Either String a`, which is used to attempt to decode or parse the extracted value.

In total, there are three section-level parsers (`section`, `sectionMb`, and `sectionDef`) and eight field-level parsers (`field`, `fieldOf`, `fieldMb`, `fieldMbOf`, `fieldDef`, `fieldDefOf`, `fieldFlag`, `fieldFlagDef`). For the `_Of` functions, `config-ini` also provides several built-in parser functions which provide nice error messages on failure.
