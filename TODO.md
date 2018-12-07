# Upstream
- [ ] Fix Dhall parser
  - [ ] Add typechecking benchmarks
- [ ] Ask how to implement `init`/`tail` in Dhall
  - [ ] Also find & replace on strings
- [ ] More helpful error in `System.Process` when `configure` doesn't exist
- [ ] Maybe fix `acid-state`?
- [ ] Investigate possible upstream bug when downloading `fltk` and unpacking
  the `.tar.gz`
- [ ] Fix tar library and/or rewrite it
# Code Maintenance
- [ ] Use a more sensible monad
- [ ] Stop being a mess
# Features
- [ ] Constraint solver
- [ ] Build/handle preprocessors
- [ ] Nix-style builds
  - [ ] Garbage collection
- [ ] Package database
- [ ] `unpack` subcommand for packages
- [ ] Cabal integration?
- [ ] Generate something like pkg-config files?
- [ ] Bootstrap "fancy" tar?
- [ ] Package "components"? Like cabal: build executables or not...
- [ ] Symlink manpages
- [ ] Completions
  - [ ] `cpkg install` should use `cpkg list`?
- [ ] Should `check-set` be hidden in help?
- [ ] Custom package directories
- [ ] Make Haskell library API better
  - [ ] Separate Dhall parts?
# Performance
- [ ] Dhall is slow
# Bugs
- [ ] `mkExe` and `makeExe` are too similar
- [ ] `glibc` example is broken
- [ ] Shouldn't be able to `cpkg dump compiler sed` since it provides no
  libraries
- [ ] Fails when symlink already exists
- [ ] Cross-compile shouldn't symlink
# Deficiencies
- [ ] `buildDepends` should be a function from operating system?
- [ ] Symlink built artifacts into a common directory (`~/.cpkg/bin`?)
- [ ] Cache package downloads globally
- [ ] No database right now
- [ ] Build step should take OS as an argument
# Packages
- [ ] Lua (`http://www.lua.org/ftp/lua-5.3.5.tar.gz`)
- [ ] jemalloc
- [ ] Cairo
- [ ] Pango
- [ ] GTK+
- [ ] GraphViz
- [ ] Ruby
- [ ] Lua
- [ ] Emacs
- [ ] DBus
- [ ] librsvg
- [ ] ImageMagick
- [ ] GHC cross-compiler?
- [ ] https://sourceforge.net/projects/infozip/files/
- [ ] https://www.gnu.org/software/coreutils/
- [ ] autoconf/automake
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
