# `config-ini`

[![Hackage](https://img.shields.io/hackage/v/config-ini.svg)](https://hackage.haskell.org/package/config-ini) ![stability: stable](https://img.shields.io/badge/stability-stable-green.svg)

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

~~~haskell
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

~~~haskell
>>> parseIniFile example configParser
Right (Config {cfNetwork = NetworkConfig {netHost = "example.com", netPort = 7878}, cfLocal = Just (LocalConfig {localUser = "terry"})})
~~~

## Bidirectional Usage

The above example had an INI file split into two sections (`NETWORK` and `LOCAL`) and a data type with a corresponding structure (containing a `NetworkConfig` and `Maybe LocalConfig` field), which allowed each `section`-level parser to construct a chunk of the configuration and then combine them. This works well if our configuration file has the same structure as our data type, but that might not be what we want. Let's imagine we want to construct our `Config` type as a flat record like this:

~~~haskell
data Config = Config
  { _cfHost :: String
  , _cfPort :: Int
  , _cfUser :: Maybe Text
  } deriving (Eq, Show)
~~~

In this case, we can't construct a `Config` value until we've parsed all three fields in two distinct subsections. One way of doing this is to return the intermediate values from our `section` parsers and construct the `Config` value at the end, once we have all three of its fields:

~~~haskell
configParser :: IniParser Config
configParser = do
  (host, port) <- section "NETWORK" $ do
    h <- fieldOf "host" string
    p <- fieldOf "port" number
    return (h, p)
  user <- section "LOCAL" $ fieldMb "user"
  return (Config host port user)
~~~

This is unfortunately awkward and repetitive. An alternative is to flatten it out by repeating invocations of `section` like below, but this has its own problems, such as unnecessary repetition of the `"NETWORK"` string literal, unnecessarily repetitive lookups, and general verbosity:

~~~haskell
configParser :: IniParser Config
configParser = do
  host <- section "NETWORK" $ fieldOf "host" string
  port <- section "NETWORK" $ fieldOf "port" number
  user <- section "LOCAL" $ fieldMb "user"
  return (Config host port user)
~~~

In situations like these, you can instead use the `Data.Ini.Config.Bidir` module, which provides a slightly different abstraction: the functions exported by this module assume that you start with a default configuration value, and parsing a field allows you to _update_ that configuration with the value of a field. The monads exported by this module have an extra type parameter that represents the type of the value being updated. The easiest way to use this module is by combining lenses with the `.=` and `.=?` operators, which take a lens and a description of a field, and produce a `SectionSpec` value that uses the provided lens to update the underlying type when parsing:

~~~haskell
makeLenses ''Config

configParser :: IniSpec Config ()
configParser = do
  section "NETWORK" $ do
    cfHost .=  field "host" string
    cfPort .=  field "port" number
  section "LOCAL" $ do
    cfUser .=? field "user"
~~~

In order to use this as a parser, we will need to provide an existing value of `Config` so we can apply our updates to it. We combine the `IniSpec` defined above with a default config

~~~haskell
configIni :: Ini Config
configIni =
  let defConfig = Config "localhost" 8080 Nothing
  in ini defConfig configParser

myParseIni :: Text -> Either String Config
myParseIni t = fmap getIniValue (parseIni t configIni)
~~~

This approach gives us other advantages, too. Each of the defined fields can be associated with some various pieces of metadata, marking them as optional for the purpose of parsing or associating a comment with them.

~~~haskell

configParser' :: IniSpec Config ()
configParser' = do
  section "NETWORK" $ do
    cfHost .=  field "host" string
      & comment ["The desired hostname"]
      & optional
    cfPort .=  field "port" number
      & comment ["The port for the server"]
  section "LOCAL" $ do
    cfUser .=? field "user"
      & comment ["The username"]
~~~

When we create an ini from this `IniSpec`, we can serialize it directly to get a "default" INI file, one which contains the supplied comments on each field. This is useful if our application wants to produce a default configuration from the same declarative specification as before.

This approach also enables another, much more powerful feature: this enables us to perform a _diff-minimal update_. You'll notice that our `parseIni` function here doesn't give us back the value directly, but rather yet another `Ini` value from which we had to extract the value. This is because the `Ini` value also records incidental formatting choices of the input file: whitespace, comments, specifics of capitalization, and so forth. When we serialize an INI file that was returned by `parseIni`, we will get out _literally the same file_ that we put in, complete with incidental formatting choices retained.

But we can also use that file and update it using the `updateIni` function: this takes a configuration value and a previous `Ini` value and builds a new `Ini` value such that as much structure as possible is retained from the original `Ini`. This means that if we parse a file, update a single field, and reserialize, that file should differ only in the field we changed _and that's it_: fields will stay in the same order (with new fields being added to the end of sections), comments will be retained, incidental whitespace will stay as it is.

This is a useful tool if you're building an application that has both a human-readable configuration as well the ability to set configuration values from within the application itself. This will allow you to rewrite the configuration file while minimizing lossy changes to a possibly-hand-edited possibly-checked-into-git configuration file.

## Combinators and Conventions

There are several variations on the same basic functionality that appear in `config-ini`. All functions that start with `section` are for parsing section-level chunks of an INI file, while all functions that start with `field` are for parsing key-value pairs within a section. Because it's reasonably common, there are also special `fieldFlag` functions which return `Bool` values, parsed in a relatively loose way.

All functions which end in `Mb` return a `Maybe` value, returning `Nothing` if the section or key was not found. All functions which end in `Def` take an additional default value, returning it if the section or key was not found. All functions which contain `Of` take a function of the type `Text -> Either String a`, which is used to attempt to decode or parse the extracted value.

In total, there are three section-level parsers (`section`, `sectionMb`, and `sectionDef`) and eight field-level parsers (`field`, `fieldOf`, `fieldMb`, `fieldMbOf`, `fieldDef`, `fieldDefOf`, `fieldFlag`, `fieldFlagDef`). For the `_Of` functions, `config-ini` also provides several built-in parser functions which provide nice error messages on failure.
