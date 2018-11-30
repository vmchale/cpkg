# Upstream
- [ ] Fix Dhall parser
  - [ ] Add typechecking benchmarks
- [ ] Ask how to implement `init`/`tail` in Dhall
- [ ] More helpful error in `System.Process` when `configure` doesn't exist
- [ ] Maybe fix `acid-state`?
- [ ] Investigate possible upstream bug when downloading `fltk` and unpacking
  the `.tar.gz`
# Features
- [ ] Constraint solver
- [ ] Build/handle preprocessors
- [ ] Cross-compilation
- [ ] Nix-style builds
  - [ ] Garbage collection
- [ ] Package database
# Deficiencies
- [ ] Symlink built artifacts into a common directory (`~/.cpkg/bin`?)
- [ ] Cache package downloads globally
- [ ] No database right now
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
# Packages
- [ ] GMP
