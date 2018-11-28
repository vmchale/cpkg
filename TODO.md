# Upstream
- [ ] Fix Dhall parser
  - [ ] Add typechecking benchmarks
- [ ] More helpful error in `System.Process` when `configure` doesn't exist
# Features
- [ ] Constraint solver
- [ ] Build/handle preprocessors
- [ ] Cross-compilation
- [ ] Nix-style builds
  - [ ] Garbage collection
- [ ] Package database
# Deficiencies
- [ ] Symlink into a common directory (`~/.cpkg/bin`?)
- [ ] Build command should take number of cores as an argument
- [ ] Should cache package downloads globally
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
