# cpkg

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
