GHC="ghc"
OUT="./out"

unigramDriver:
	${GHC} -outputdir=${OUT} -o unigramDriver.out ./Unigram/Driver.hs
