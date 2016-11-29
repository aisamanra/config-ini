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

## Setter- and Lens-Based Usage

The above example had an INI file split into two sections (`NETWORK` and `LOCAL`) and a data type with a corresponding structure (containing a `NetworkConfig` and `Maybe LocalConfig` field), which allowed each `section`-level parser to construct a chunk of the configuration and then combine them. This works well if our configuration file has the same structure as our data type, but that might not be what we want. Let's imagine we want to construct our `Config` type as a flat record like this:

~~~.haskell
data Config = Config
  { _cfHost :: String
  , _cfPort :: Int
  , _cfUser :: Maybe Text
  } deriving (Eq, Show)
~~~

In this case, we can't construct a `Config` value until we've parsed all three fields in two distinct subsections. One way of doing this is to return the intermediate values from our `section` parsers and construct the value at the end:

~~~.haskell
configParser :: IniParser Config
configParser = do
  (host, port) <- section "NETWORK" $ do
    h <- fieldOf "host" string
    p <- fieldOf "port" number
    return (h, p)
  user <- section "LOCAL" $ fieldMb "user"
  return (Config host port user)
~~~

This is awkward and repetitive. We could flatten it out by using the same `section` parser multiple times, but this has its own problems, such as unnecessary repetition of the `"NETWORK"` string literal, unnecessarily repetitive table lookups, and general verbosity:

~~~.haskell
configParser :: IniParser Config
configParser = do
  host <- section "NETWORK" $ fieldOf "host" string
  port <- section "NETWORK" $ fieldOf "port" number
  user <- section "LOCAL" $ fieldMb "user"
  return (Config host port user)
~~~

One way of resolving this is to use the `Data.Ini.Config.St` module, which provides a slightly different abstraction: the functions exported by this module assume that you start with a default configuration and each field acts as an update on that underlying value. The monads in this module have an extra type parameter that represents the value being modified. The easiest way to use this is with lenses and the `.=` and `.=?` operators, which take a lens and a normal `SectionParser` value, and produce a `SectionStParser` value that uses the lens to update the underlying type:

~~~.haskell
makeLenses ''Config

configParser :: IniStParser Config ()
configParser = do
  sectionSt "NETWORK" $ do
    cfHost .= fieldOf "host" string
    cfPort .= fieldOf "port" number
  sectionSt "LOCAL" $ do
    cfUser .= fieldMb "user"
~~~

In order to use this parser, we will need to provide an existing value of `Config` so we can apply our updates to it. This is a downside to this approach: in this case, even though the `host` and `port` fields are obligatory, we need to provide dummy values for them.

~~~.haskell
myParseIni :: Text -> Either String Config
myParseIni t = parseIniFileSt t defaultConfig configParser
  where defaultConfig = Config "unset" 0 Nothing
~~~

The implementation isn't tied to lenses, and many of the functions exported by `Data.Ini.Config.St` expected any generic setter, and not a lens specifically. If we didn't want to use lenses specifically, we can still take advantage of this library in a more verbose way:

~~~.haskell
configParser :: IniStParser Config ()
configParser = do
  sectionSt "NETWORK" $ do
    fieldOfSt "host" string (\ h s -> s { _cfHost = h })
    fieldOfSt "port" number (\ p s -> s { _cfPort = p })
  sectionSt "LOCAL" $ do
    fieldMbSt "user" (\ u s -> s { _cfUser = u })
~~~

## Combinators and Conventions

There are several variations on the same basic functionality that appear in `config-ini`. All functions that start with `section` are for parsing section-level chunks of an INI file, while all functions that start with `field` are for parsing key-value pairs within a section. Because it's reasonably common, there are also special `fieldFlag` functions which return `Bool` values, parsed in a relatively loose way.

All functions which end in `Mb` return a `Maybe` value, returning `Nothing` if the section or key was not found. All functions which end in `Def` take an additional default value, returning it if the section or key was not found. All functions which contain `Of` take a function of the type `Text -> Either String a`, which is used to attempt to decode or parse the extracted value.

In total, there are three section-level parsers (`section`, `sectionMb`, and `sectionDef`) and eight field-level parsers (`field`, `fieldOf`, `fieldMb`, `fieldMbOf`, `fieldDef`, `fieldDefOf`, `fieldFlag`, `fieldFlagDef`). For the `_Of` functions, `config-ini` also provides several built-in parser functions which provide nice error messages on failure.
