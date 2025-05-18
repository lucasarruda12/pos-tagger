GHC="ghc"
OUT="./out"

default:
	echo "sorry"

# Util
rt:
	${GHC} -outputdir=${OUT} -o rt.out ./Util/RemoveTags.hs

cm:
	${GHC} -outputdir=${OUT} -o cm.out ./Util/BuildConfusionMatrix.hs

# Unigram
stupidUnigramTrainer:
	${GHC} -outputdir=${OUT} -o stupidUnigramTrainer.out ./Unigram/stupid/Trainer.hs

stupidUnigramTagger:
	${GHC} -outputdir=${OUT} -o stupidUnigramTagger.out ./Unigram/stupid/Tagger.hs

unigramTrainer:
	${GHC} -outputdir=${OUT} -o unigramTrainer.out ./Unigram/unknown/Trainer.hs

unigramTagger:
	${GHC} -outputdir=${OUT} -o unigramTagger.out ./Unigram/Tagger.hs

# Bigram
stupidBigramTrainer:
	${GHC} -outputdir=${OUT} -o stupidBigramTrainer.out ./Bigram/stupid/Trainer.hs

stupidBigramTagger:
	${GHC} -outputdir=${OUT} -o stupidBigramTagger.out ./Bigram/stupid/Tagger.hs
