# Upstream
- [ ] Fix Dhall parser
  - [ ] Add typechecking benchmarks
- [ ] Ask how to implement `init`/`tail` in Dhall
  - [ ] Also find & replace on strings
- [ ] Maybe fix `acid-state`?
- [ ] Investigate possible upstream bug when downloading `fltk` and unpacking
  the `.tar.gz`
- [ ] Fix tar library and/or rewrite it
- [x] Dhall space leak
  - [ ] Dhall performance problems
- [ ] Fix `hp2pretty` completions to work with `.hp` files alone
- [ ] Investigate performance problems while decoding `index.bin`
- [ ] complain about ncurses 6.1 not cross-compiling
- [ ] complain about Xorg libraries and bad defaults (malloc) related to
  cross-compiling
- [ ] definitely complain about intltool
- [ ] complain about m4 (?)
- [ ] `hsc2hs` takes too long when cross-compiling
- [ ] `cabal new-build -v4` needs better error message
- [ ] Figure out why `libX11` stalls when not watched (i.e. when `-vv` is not
  passed...)
- [ ] `xextproto` doesn't like `aarch64`
# Libraries
- [ ] Consider splitting off library for parsing triples &c.
# Code Maintenance
- [ ] Use a more sensible monad
- [ ] Stop being a mess
# Documentation
- [ ] Document the difference between `symlinkBinary` and `symlinkLibrary`...
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
- [ ] Allow a package's function to specify a `pkg-config` dir, etc.
- [ ] Bootstrap "fancy" tar?
- [ ] Package "components"? Like cabal: build executables or not...
- [ ] Symlink manpages
- [ ] Completions
  - [ ] `cpkg install` should use `cpkg list`?
- [ ] Custom package sets
- [ ] Haskell library API should be better
- [ ] `cpkg list` should show what binaries it provides
- [ ] `test` should map to commands such as `make check`; should not be run when
  cross-compiling
- [ ] `pkgRuntimeDeps` field?
# Performance
- [ ] Dhall is slow
- [ ] Space leak due to zygomorphism?
# Bugs
- [ ] Shouldn't be able to `cpkg dump compiler sed` since it provides no
  libraries
- [ ] Figure out `ACLOCAL_PATH`
- [ ] Fails when symlink already exists
- [ ] Cross-compile shouldn't symlink
- [ ] If X depends on Y, Z and Y depends on Z, then it will not link things
  correctly...
- [ ] Figure out `PERL5LIB` variable
# Deficiencies
- [ ] Get rid of `symlinkLibrary`? And just use `symlink`...
- [ ] Cache package downloads globally
- [ ] No database w/ graph right now
- [ ] Build step should take OS as an argument
- [ ] Make e.g. `vim` depend on `glibc`
- [ ] Get rid of annoying `LD_PRELOAD` hack?
- [ ] dhall library should prefer `symlink` to `copyFile` when possible? hmm
# Packages
- [ ] bash
- [ ] jemalloc
- [ ] GTK+
- [ ] GraphViz
- [ ] Ruby
- [ ] librsvg
- [ ] https://github.com/linux-noah/noah
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
- [ ] https://ftp.gnu.org/gnu/texinfo/ ?
- [ ] charset (?)
- [ ] https://sourceware.org/newlib/
- [ ] https://pypi.org/project/scour/
- [ ] libtizcore
- [ ] https://cairosvg.org/
- [ ] ghostscript
- [ ] http://www.linuxfromscratch.org/blfs/view/svn/general/boost.html
- [ ] brlcad https://jaist.dl.sourceforge.net/project/brlcad/BRL-CAD%20Source/7.28.0/brlcad-7.28.0.tar.bz2
- [ ] ragel
- [ ] https://ftp.gnu.org/gnu/parallel/parallel-20181222.tar.bz2
- [ ] http://savannah.gnu.org/projects/patch/
- [ ] Blender https://download.blender.org/source/blender-2.79b.tar.gz
- [ ] http://www.ibiblio.org/pub/Linux/utils/file/symlinks-1.4.tar.gz
- [ ] http://www.linuxfromscratch.org/blfs/view/8.2/general/tree.html
- [ ] libsigsegv
- [ ] weirdo levenshtein distance in ATS
- [ ] https://github.com/jmcnamara/libxlsxwriter/archive/RELEASE_0.8.4.tar.gz
- [ ] https://github.com/gildor2/fast_zlib
- [ ] grep
# Documentation
- [ ] Write a manpage
- [ ] Documentation in
  - [ ] French
  - [ ] German
- [ ] Write `info` thing like `info sed`
# Dhall Libraries
- [ ] `mkExe` and `makeExe` are too similar
# Tests
