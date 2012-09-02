
.PHONY: all configure build test install clean doc

all: build

doc: configure
	cabal-dev haddock

install: build
	-cabal-dev ghc-pkg unregister cookie-jar
	cabal-dev install

configure:
	cabal-dev configure --enable-tests

build: configure
	cabal-dev build

test: build
	cabal-dev test

clean:
	cabal-dev clean

