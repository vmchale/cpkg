# cpkg

## 0.2.5.8

  * Fix something for `base` 4.18.*

## 0.2.5.0

  * Add `package set:` info to `dhall --version`

## 0.2.4.2

  * Support `aarch64`

## 0.2.4.1

  * Use `bz2` in place of `bzlib` for now

## 0.2.4.0

  * Use [`libarchive`](http://hackage.haskell.org/package/libarchive) exclusively

## 0.2.3.8

  * Support `.zst` via [zstd](http://hackage.haskell.org/package/zstd) library
  * Pin new Dhall package set

## 0.2.3.7

  * Pin package

## 0.2.3.6

  * Use `dhall` 1.27.0

## 0.2.3.5

  * Use `lzlib` 0.3.0.0

## 0.2.3.4

  * Use `libarchive` 2.0.0.0

## 0.2.3.3

  * Add `find` subcommand

## 0.2.3.2

  * Add `.lz` support

## 0.2.3.1

  * Now works with Dhall 1.25.0 on a sufficiently new GHC

## 0.2.3.0

  * Add `garbage-collect` subcommand
  * Add `uninstall` subcommand
  * Add `nuke-cache` subcommand

## 0.2.2.0

  * Add `printLdLibFlags` function and add functionality to CLI interface
  * `cfg.installDir` is now absolute

## 0.2.1.0

  * Add `SymlinkManpage` command

## 0.2.0.1

  * Pass `-p0` option to `patch`

## 0.2.0.0

  * Support more MIPS architectures

## 0.1.3.1

  * Don't install build-tool dependencies globally

## 0.1.3.0

  * Add ability to patch libraries

## 0.1.2.1

  * Add `--global` flag

## 0.1.2.0

  * Export `Dep`

## 0.1.1.1

  * Stream using `libarchive` lazily

## 0.1.1.0

  * Export `EnvVar`
  * Better diagnostic output
  * Fix bug where cross dependencies' `bin/` directory was added to `PATH` for
    subsequent package builds
  * Allow use of `libarchive` for packages that cannot be handled with the `tar`
    library

## 0.1.0.0

Initial release
