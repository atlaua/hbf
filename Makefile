.PHONY: *

all: hbf

hbf:
	ghc --make -O2 hbf.hs

clean:
	bash -c 'shopt -s globstar; rm -f hbf test/{bench,math} **/*.{hi,o}'

test:
	ghc --make -O2 test/math.hs
	test/math

bench:
	ghc --make -O2 test/bench.hs
	test/bench opt
