GHC="ghc"
OUT="./out"

default:
	echo "sorry"

# Util
rt:
	${GHC} -outputdir=${OUT} -O2 -o rt.out ./Util/RemoveTags.hs
cm:
	${GHC} -outputdir=${OUT} -O2 -o cm.out ./Util/BuildConfusionMatrix.hs

# Unigram
stupidUnigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o stupidUnigramTrainer.out ./Unigram/stupid/Trainer.hs
stupidUnigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o stupidUnigramTagger.out ./Unigram/stupid/Tagger.hs

unigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o unigramTrainer.out ./Unigram/unknown/Trainer.hs
unigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o unigramTagger.out ./Unigram/Tagger.hs

# Bigram
stupidBigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o stupidBigramTrainer.out ./Bigram/stupid/Trainer.hs
stupidBigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o stupidBigramTagger.out ./Bigram/stupid/Tagger.hs

bigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o bigramTrainer.out ./Bigram/Trainer.hs
bigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o bigramTagger.out ./Bigram/Tagger.hs

# Tag-Considering Bigram
tagBigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o tagBigramTrainer.out ./TagBigram/Trainer.hs

tagBigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o tagBigramTagger.out ./TagBigram/Tagger.hs

# Trigram
stupidTrigramTrainer:
	${GHC} -outputdir=${OUT} -O2 -o trigramTrainer.out ./Trigram/stupid/Trainer.hs
stupidTrigramTagger:
	${GHC} -outputdir=${OUT} -O2 -o trigramTagger.out ./Trigram/stupid/Tagger.hs

# Trigram with Backoff
backoffTrainer:
	${GHC} -outputdir=${OUT} -O2 -o backoffTrainer.out ./Backoff/Trainer.hs
backoffTagger:
	${GHC} -outputdir=${OUT} -O2 -o backoffTagger.out ./Backoff/Tagger.hs
