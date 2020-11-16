.PHONY: build publish
build:
	mdbook build
publish: build
	git add -A
	git commit -m "update $$(date +%Y/%m/%d-%H:%M:%S)"
	git push origin develop
