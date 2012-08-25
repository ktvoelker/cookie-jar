
.PHONY: all configure build install clean doc

all: build

doc: configure
	cabal-dev haddock

install: build
	-cabal-dev ghc-pkg unregister cookie-jar
	cabal-dev install

configure:
	cabal-dev configure

build: configure
	cabal-dev build

clean:
	cabal-dev clean

