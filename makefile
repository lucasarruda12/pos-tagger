GHC="ghc"
OUT="./out"

default:
	echo "sorry. No default"

# Util
tester:
	${GHC} -outputdir=${OUT} -o tester.out ./Util/Tester.hs

FindLonelyWords:
	${GHC} -outputdir=${OUT} -o FindLonelyWords.out ./Util/replaceUnknownWords/FindLonelyWords.hs

# Unigram
unigramDriver:
	${GHC} -outputdir=${OUT} -o unigramDriver.out ./Unigram/Driver.hs

unigramTagger:
	${GHC} -outputdir=${OUT} -o unigramTagger.out ./Unigram/Tagger.hs


