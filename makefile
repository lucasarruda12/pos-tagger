GHC="ghc"
OUT="./out"

default:
	echo "sorry"

# Util
Tester:
	${GHC} -outputdir=${OUT} -o tester.out ./Util/Tester.hs

BuildConfusionMatrix:
	${GHC} -outputdir=${OUT} -o bcm.out ./Util/ConfusionMatrix.hs

# Unigram
stupidUnigramDriver:
	${GHC} -outputdir=${OUT} -o unigramDriver.out ./Unigram/stupid/Driver.hs

stupidUnigramTagger:
	${GHC} -outputdir=${OUT} -o unigramTagger.out ./Unigram/stupid/Tagger.hs

unkUnigramDriver:
	${GHC} -outputdir=${OUT} -o unigramDriver.out ./Unigram/unknown/Driver.hs

unkUnigramTagger:
	${GHC} -outputdir=${OUT} -o unigramTagger.out ./Unigram/unknown/Tagger.hs
