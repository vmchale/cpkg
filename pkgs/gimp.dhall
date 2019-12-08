let gimp =
  let mkLDFlagsGimp =
    λ(linkDirs : List Text) →
      concatMapSep " " Text (λ(dir : Text) → "-L${dir}") linkDirs
  in
  let gimpEnv =
    λ(perlV : List Natural) →
    λ(cfg : types.BuildVars) →
      prelude.defaultPath cfg # [ { var = "LDFLAGS", value = (mkLDFlagsGimp cfg.linkDirs) ++ " -lpcre -lfribidi" }
                                , prelude.mkPkgConfigVar cfg.linkDirs
                                , prelude.mkPerlLib { libDirs = cfg.linkDirs, perlVersion = perlV, cfg = cfg }
                                , prelude.mkLDPreload cfg.preloadLibs
                                -- , prelude.mkPyPath [2,7] cfg.linkDirs
                                -- , prelude.mkXdgDataDirs cfg.shareDirs
                                -- , prelude.mkLDFlags cfg.linkDirs
                                -- , prelude.mkCFlags cfg
                                -- , prelude.libPath cfg
                                -- , prelude.mkLDRunPath cfg.linkDirs
                                ]
  in
  let gimpConfig =
    λ(perlV : List Natural) →
    λ(cfg : types.BuildVars) →
      [ prelude.mkExe "configure"
      , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                            , arguments = [ "--prefix=${cfg.installDir}" ]
                                            , environment = Some (gimpEnv perlV cfg)
                                            })
      ]
  in

  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    mkGimpPackage "gimp" x ⫽
      { pkgUrl = "https://download.gimp.org/mirror/pub/gimp/v${versionString}/gimp-${fullVersion}.tar.bz2"
      , pkgBuildDeps = [ prelude.lowerBound { name = "intltool", lower = [0,40,1] }
                       , prelude.unbounded "gettext"
                       , prelude.lowerBound { name = "python2", lower = [2,5] }
                       , prelude.lowerBound { name = "perl", lower = [5,8,1] }
                       ]
      , pkgDeps = [ prelude.lowerBound { name = "babl", lower = [0,1,66] }
                  , prelude.lowerBound { name = "gegl", lower = [0,4,16] }
                  , prelude.lowerBound { name = "atk", lower = [2,2,0] }
                  , prelude.lowerBound { name = "glib", lower = [2,54,2] }
                  , prelude.lowerBound { name = "xz", lower = [5,0,0] }
                  , prelude.lowerBound { name = "gtk2", lower = [2,24,32] }
                  , prelude.lowerBound { name = "gexiv2", lower = [0,10,6] }
                  , prelude.lowerBound { name = "libmypaint", lower = [1,3,0] }
                  , prelude.lowerBound { name = "librsvg", lower = [2,40,6] }
                  -- , prelude.unbounded "glib-networking"
                  -- , prelude.lowerBound { name = "gdk-pixbuf", lower = [2,30,8] }
                  -- , prelude.lowerBound { name = "cairo", lower = [1,12,2] }
                  -- , prelude.lowerBound { name = "fontconfig", lower = [2,12,4] }
                  -- , prelude.lowerBound { name = "lcms2", lower = [2,8] }
                  -- , prelude.lowerBound { name = "pygtk", lower = [2,10,4] }
                  -- , prelude.lowerBound { name = "pycairo", lower = [1,0,2] }
                  -- , prelude.unbounded "libwebp"
                  , prelude.unbounded "libjpeg-turbo"
                  , prelude.unbounded "libtiff"
                  ]
      , configureCommand = gimpConfig [5,30,0] -- TODO: take as argument
      }
in

