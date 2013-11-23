.PHONY: *

all: hbf

hbf:
	ghc --make -O2 hbf.hs

test:
	ghc --make -O2 test/math.hs
	test/math

clean:
	bash -c 'shopt -s globstar; rm hbf test/math **/*.{hi,o}'
