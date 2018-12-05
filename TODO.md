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
# Performance
- [ ] Dhall is slow
# Bugs
- [ ] `mkExe` and `makeExe` are too similar
- [ ] `glibc` example is broken
# Deficiencies
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
# Documentation
- [ ] Write a manpage
- [ ] Documentation in
  - [ ] French
  - [ ] German
# Dhall Libraries
- [ ] Use something other than `Optional Text` for target architectures
# Tests
- [ ] Nontrivial example where we have to link w/ another package installed by
  `cpkg`.
- [ ] Figure out something w/ Lua
  - [ ] Use `sed` to delete 13th line
  - [ ] Then set `INSTALL_TOP` using an environment var?
# Packages
- [ ] GMP
