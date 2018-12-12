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
- [ ] `cpkg list` should show what binaries it provides
- [ ] https://fukuchi.org/works/qrencode/
# Performance
- [ ] Dhall is slow
# Bugs
- [ ] Shouldn't be able to `cpkg dump compiler sed` since it provides no
  libraries
- [ ] Fails when symlink already exists
- [ ] Cross-compile shouldn't symlink
- [ ] Don't rebuild `ncurses` when rebuilding `vim` with `lua` - `ncurses`
  does not depend on lua so its hash/include dirs should not be changed
- [ ] If X depends on Y, Z and Y depends on Z, then it will not link things
  correctly...
# Deficiencies
- [ ] `mkExe` and `makeExe` are too similar
- [ ] Cache package downloads globally
- [ ] No database w/ graph right now
- [ ] Build step should take OS as an argument
- [ ] Make e.g. `vim` depend on `glibc`
# Packages
- [ ] jemalloc
- [ ] Pango
- [ ] GTK+
- [ ] GraphViz
- [ ] Ruby
- [ ] librsvg
- [ ] ImageMagick
- [ ] GHC cross-compiler?
- [ ] https://sourceforge.net/projects/infozip/files/
- [ ] https://www.gnu.org/software/coreutils/
- [ ] fdupes
- [ ] Figlet
- [ ] GHC (`https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-src.tar.xz`)
  & cross-compiler
- [ ] `http://prdownloads.sourceforge.net/ctags/ctags-5.8.tar.gz`
- [ ] https://ftp.gnu.org/gnu/coreutils/coreutils-8.30.tar.xz
- [ ] https://www.libarchive.org/downloads/libarchive-3.3.3.tar.gz
- [ ] `patscc`
- [ ] `rustc`
- [ ] https://github.com/ZBar/ZBar
- [ ] SDL2
- [ ] Doxygen
- [ ] libexif
- [ ] LibTIFF
- [ ] FFmpeg
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
