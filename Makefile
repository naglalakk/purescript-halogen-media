PCK_MANAGER=yarn

.PHONY: all test example clean

install:
	spago install && $(PCK_MANAGER) install

example:
	make bundle && http-server -o

bundle:
	spago build --path 'example/**/*.purs' && spago bundle-app --main Example.Main --to example/dist/index.js

test:
	spago test

clean:
	rm -rf output generated-docs
