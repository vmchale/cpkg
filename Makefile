.PHONY: ci

ci: .github/workflows/dhall.yml

.github/workflows:
	mkdir -p $@

.github/workflows/dhall.yml: self-ci.dhall .github/workflows
	dhall-to-yaml --file $< --output $@
