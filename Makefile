MAIN=yasc.hs
all: yasc

yasc: src/$(MAIN)
	ghc -o $@ --make $^
