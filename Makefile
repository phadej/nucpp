.PHONY: all build install

all: build

build:
	cabal build

install: build
	cp $$(cabal-plan list-bin nucpp) /cabal/bin

