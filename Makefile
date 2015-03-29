default: test/output.pedantic test/output.normal

install:
	cabal sandbox init
	cabal install

test/output.pedantic: install
	cabal run test.pedantic > test/output.pedantic

test/output.normal: install
	cabal run test.normal > test/output.normal
