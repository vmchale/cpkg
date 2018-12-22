# Upstream
- [ ] Fix Dhall parser
  - [ ] Add typechecking benchmarks
- [ ] Ask how to implement `init`/`tail` in Dhall
  - [ ] Also find & replace on strings
- [ ] Maybe fix `acid-state`?
- [ ] Investigate possible upstream bug when downloading `fltk` and unpacking
  the `.tar.gz`
- [ ] Fix tar library and/or rewrite it
# Code Maintenance
- [ ] Use a more sensible monad
- [ ] Stop being a mess
# Features
- [ ] Pass verbosity to configuration/build script
- [ ] Constraint solver
- [ ] Build/handle preprocessors
  - [ ] Don't try to use cross-compiler on preprocessors
- [x] Nix-style builds
  - [ ] Garbage collection
- [ ] Caching
  - [ ] Cache global package set?
- [ ] `unpack` subcommand for packages
- [ ] Cabal integration?
- [ ] Generate pkg-config files?
- [ ] Allow a package's function to specify a `pkg-config` dir, etc.
- [ ] Bootstrap "fancy" tar?
- [ ] Package "components"? Like cabal: build executables or not...
- [ ] Symlink manpages
- [ ] Completions
  - [ ] `cpkg install` should use `cpkg list`?
- [ ] Custom package sets
- [ ] Make Haskell library API better
- [ ] `cpkg list` should show what binaries it provides
- [ ] `test` should map to commands such as `make check`; should not be run when
  cross-compiling
- [ ] `wrap-cabal` subcommand to wrap cabal with a given set of packages?
- [ ] `pkgRuntimeDeps` field?
# Performance
- [ ] Dhall is slow
# Bugs
- [ ] Shouldn't be able to `cpkg dump compiler sed` since it provides no
  libraries
- [ ] Figure out `ACLOCAL_PATH` and force it to use local m4
- [ ] Fails when symlink already exists
- [ ] Cross-compile shouldn't symlink
- [ ] Don't rebuild `ncurses` when rebuilding `vim` with `lua` - `ncurses`
  does not depend on lua so its hash/include dirs should not be changed
- [ ] If X depends on Y, Z and Y depends on Z, then it will not link things
  correctly...
- [ ] Figure out `PERL5LIB` variable
# Deficiencies
- [ ] `mkExe` and `makeExe` are too similar
- [ ] Cache package downloads globally
- [ ] No database w/ graph right now
- [ ] Build step should take OS as an argument
- [ ] Make e.g. `vim` depend on `glibc`
# Packages
- [ ] jemalloc
- [ ] GTK+
- [ ] GraphViz
- [ ] Ruby
- [ ] librsvg
- [ ] ImageMagick
- [ ] GHC cross-compiler?
- [ ] https://sourceforge.net/projects/infozip/files/
- [ ] fdupes
- [ ] Figlet
- [ ] GHC (`https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-src.tar.xz`)
  & cross-compiler
- [ ] `http://prdownloads.sourceforge.net/ctags/ctags-5.8.tar.gz`
- [ ] https://www.libarchive.org/downloads/libarchive-3.3.3.tar.gz
- [ ] `patscc`
- [ ] `rustc`
- [ ] https://github.com/ZBar/ZBar
- [ ] Doxygen
- [ ] libexif
- [ ] LibTIFF
- [ ] FFmpeg
- [ ] intltool
- [ ] TCmalloc
# Documentation
- [ ] Write a manpage
- [ ] Documentation in
  - [ ] French
  - [ ] German
- [ ] Write `info` thing like `info sed`
# Dhall Libraries
- [ ] Use something other than `Optional Text` for target architectures
- [ ] Fix `makeGnuExe` to install binaries
# Tests
- [ ] Nontrivial example where we have to link w/ another package installed by
  `cpkg`.
  - [ ] GPG
- [ ] Figure out something w/ Lua
  - [ ] Use `sed` to delete 13th line
  - [ ] Then set `INSTALL_TOP` using an environment var?
# Packages
- [ ] GMP
