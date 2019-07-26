PCK_MANAGER=yarn

.PHONY: all test example clean

install:
	spago install && $(PCK_MANAGER) install

example:
	make browser && http-server -o
browser:
	make bundle && parcel build example/build/index.js -d example/dist

bundle:
	spago build --path 'example/**/*.purs' && spago bundle-app --main Example.Main --to example/build/index.js

test:
	spago test

clean:
	rm -rf output generated-docs
