0.2.4.0
=======

- Fixed a bug that prevented `config-ini` from building with GHC 7.10
- Bumped version bounds for `containers` to enable GHC 8.6 compat

0.2.3.0
=======

- Add the `iniValueL` lens for access to the underlying INI value in a
  value of type `Ini s`
- Bumped compatible version of `megaparsec`

0.2.2.0
=======

- Added `sections`, `sectionOf`, and `sectionsOf` helpers to the
  vanilla API for more flexibility in working with section names
- Put `test-doctest` behind a flag, which is disabled by default

0.2.1.1
=======

- Fix doctest pointing at deprecated API

0.2.1.0
=======

- Fix regression in standard API where values would be reported with
  extraneous whitespace

0.2.0.1
=======

- Include prewritten test cases in distributed package

0.2.0.0
=======

- Introduced `Data.Config.Ini.Bidir`, which introduces a new alternate
  API for working with Ini files.
- Reworked the internal representation to accomodate
  `Data.Config.Ini.Bidir`; as such, the structure of
  `Data.Config.Ini.Raw` is radically changed
- Dropped GHC 7.8 backwards-compatibility.

0.1.2.1
=======

- GHC 8.2 compatibility

0.1.2.0
=======

- GHC 7.8 backwards-compatibility
- Started changelog
