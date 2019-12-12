.PHONY: ci

ci: .github/workflows/dhall.yml .github/workflows/haskell.yml

.github/workflows:
	mkdir -p $@

.github/workflows/dhall.yml: dhall-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@

.github/workflows/haskell.yml: haskell-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@
