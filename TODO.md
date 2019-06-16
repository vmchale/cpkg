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
- [ ] Figure out why `libX11` stalls when not watched (i.e. when `-vv` is not
  passed...)
- [ ] figure out why `libsepol` doesn't cross-compile
- [ ] `xextproto` doesn't like `aarch64`
- [ ] ImageMagick fails on ARM
- [ ] `wget` doesn't work with latest gnutls
- [ ] Report bug to LFS: libevent depends on openssl as they have configured it...
- [ ] OpenSSH fails to cross-compile
- [ ] bz2 fails to unpack glu?
# Libraries
- [ ] Consider splitting off library for parsing triples &c.
# Code Maintenance
- [ ] Use a more sensible monad
- [ ] Stop being a mess
# Documentation
- [ ] Document use of `cabal.project` files (which is simpler...)
# Features
- [ ] Something like cabal flags - enable exif with feh, for example...
- [ ] Pass verbosity to configuration/build script
- [ ] Constraint solver
- [ ] Build/handle preprocessors
  - [ ] Don't try to use cross-compiler on preprocessors
- [x] Nix-style builds
  - [ ] Garbage collection
- [ ] Caching
  - [ ] Cache global package set?
- [ ] `unpack` subcommand for packages
- [ ] Cabal integration? Make it usable as a package for `Setup.hs` files :o
  - [ ] Better Haddock API; expose more types
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
- [ ] append to `cabal.project` file
- [ ] pass `pi@32.32.43.00` or whatever on the command-line?
- [ ] Add IA64 (itanium) support
- [ ] Add HPUX/HPUX64 support
- [ ] Store build logs - options/environment variables, etc.
# Performance
- [ ] Dhall is slow
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
# Testing
- [ ] Maintainer subcommand to build every single package
# Packages
- [ ] bash
- [ ] Ruby
- [ ] librsvg
- [ ] https://github.com/linux-noah/noah
- [ ] GHC cross-compiler?
- [ ] https://sourceforge.net/projects/infozip/files/
- [ ] fdupes
- [ ] Figlet
- [ ] GHC (`https://downloads.haskell.org/~ghc/8.6.3/ghc-8.6.3-src.tar.xz`)
  & cross-compiler
- [ ] weighttp
- [ ] `patscc`
- [ ] `rustc`
- [ ] https://github.com/ZBar/ZBar
- [ ] Doxygen
- [ ] exif, libexif-gtk, gexif (https://libexif.github.io/)
- [ ] nomacs
- [ ] FFmpeg
- [ ] https://ftp.gnu.org/gnu/texinfo/ ?
- [ ] charset (?)
- [ ] weighttpd https://github.com/lighttpd/weighttp
- [ ] https://sourceware.org/newlib/
- [ ] fish
- [ ] libtizcore
- [ ] ghostscript
- [ ] https://github.com/yaml/libyaml
- [ ] https://mosh.org/mosh-1.3.2.tar.gz
- [ ] http://www.linuxfromscratch.org/blfs/view/svn/general/boost.html
- [ ] brlcad https://jaist.dl.sourceforge.net/project/brlcad/BRL-CAD%20Source/7.28.0/brlcad-7.28.0.tar.bz2
- [ ] gstreamer-video
- [ ] ragel
- [ ] https://ftp.gnu.org/gnu/parallel/parallel-20181222.tar.bz2
- [ ] http://savannah.gnu.org/projects/patch/
- [ ] Blender https://download.blender.org/source/blender-2.79b.tar.gz
- [ ] http://www.ibiblio.org/pub/Linux/utils/file/symlinks-1.4.tar.gz
- [ ] http://www.linuxfromscratch.org/blfs/view/8.2/general/tree.html
- [ ] libsigsegv
- [ ] pyqt5
- [ ] weirdo levenshtein distance in ATS
- [ ] https://github.com/jmcnamara/libxlsxwriter/archive/RELEASE_0.8.4.tar.gz
- [ ] http://www.linuxfromscratch.org/blfs/view/8.3/postlfs/cracklib.html
- [ ] https://github.com/gildor2/fast_zlib
- [ ] grep
- [ ] https://github.com/chapel-lang/chapel/releases/download/1.18.0/chapel-1.18.0.tar.gz (https://chapel-lang.org/docs/usingchapel/QUICKSTART.html)
- [ ] ed
- [ ] cloc
- [ ] libpeas
- [ ] libcaca
- [ ] alarm, pread, top, htop, kill, grep
- [ ] http://www.linuxfromscratch.org/blfs/view/8.1/pst/xmlto.html
- [ ] https://liquidtelecom.dl.sourceforge.net/project/schilytools/schily-2018-12-21.tar.bz2
- [ ] https://ftp.gnu.org/gnu/autogen/rel5.18.16/
- [ ] https://github.com/pkgconf/pkgconf
- [ ] http://www.linuxfromscratch.org/lfs/view/6.6/appendices/dependencies.html
- [ ] https://github.com/scipy/scipy/releases/download/v1.2.0/scipy-1.2.0.tar.xz
- [ ] Perl wrapper like `mkPy3Wrapper`
- [ ] http://www.linuxfromscratch.org/blfs/view/cvs/basicnet/glib-networking.html
- [ ] http://www.linuxfromscratch.org/blfs/view/cvs/gnome/gexiv2.html
- [ ] http://www.linuxfromscratch.org/blfs/view/8.3/x/x7driver.html#libinput
- [ ] http://luajit.org/index.html
- [ ] https://github.com/mquinson/po4a
- [ ] texinfo/help2man
- [ ] libutemper
- [ ] https://www.gimp.org/source/#gimp-requirements
- [ ] https://github.com/GNOME/cheese
- [ ] http://trousers.sourceforge.net/
- [ ] https://www.gnu.org/software/libidn/#libidn2
- [ ] https://nlnetlabs.nl/projects/unbound/about/
- [ ] https://www.gnupg.org/ftp/gcrypt/pinentry/pinentry-1.1.0.tar.bz2
- [ ] http://aa-project.sourceforge.net/aalib/
- [ ] https://www.gnupg.org/ftp/gcrypt/ntbtls/ntbtls-0.1.2.tar.bz2
- [ ] https://github.com/GNOME/libgudev
- [ ] nss?
- [ ] libXcursor
- [ ] scrot (https://github.com/dreamer/scrot/releases)
- [ ] http://download.redis.io/releases/redis-5.0.3.tar.gz
- [ ] Rarian
- [ ] https://github.com/garabik/unicode/releases
- [ ] evince
- [ ] https://github.com/GNOME/libgweather
- [ ] GIMP
- [ ] https://github.com/mtoyoda/sl/releases
- [ ] glib-networking
- [ ] libsocketcap
- [ ] https://github.com/pinard/paxutils/releases
- [ ] https://github.com/aperezdc/signify
- [ ] zstd
- [ ] lz4
- [ ] oclint
- [ ] pdftotext http://www.xpdfreader.com/download.html
- [ ] http://www.linuxfromscratch.org/blfs/view/8.3/kde/okular5.html
# Documentation
- [ ] Write a manpage
- [ ] Documentation in
  - [ ] French
  - [ ] German
- [ ] Write `info` thing like `info sed`
# Dhall Libraries
- [ ] `mkExe` and `makeExe` are too similar
# Tests
