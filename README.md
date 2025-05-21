# Part-of-Speech Tagger\

n-gram based part of speech tagger. Each directory encapsulates tools to "train a model" and tag penn-treebank-formatted input. There are also tools to remove tags from a already tagged corpus and build a confusion matrix based on two tagged versions of the same corpus, these tools are inside the Utils directory.

This is my first large-ish haskell project, so a lot of it is terrible to look at. For that i am sorry.

## Compiling

For now, you can take a look at the makefile.

## Running

- For the trainers: they expect to be fed "POS_TAG POS_TAG POS_TAG\n" lines from stdin, and will spit out "undescore-sparated-n-grams_TAG\n" to stdout. So, a full call would look like:

```bash
$ cat {trainset} | Trainer.out > learned.data
```

- For the taggers: they expect a file name as argument, from which they will read the trained data, and then "POS POS POS POS\n" from stdin. It they will spit out "POS_TAG POS_TAG POS_TAG\n" to stdout.

```bash
Tagger.out {learned.data}
```

- For the removeTags utility: It expect "POS_TAG POS_TAG POS_TAG" from stdin and will spit out "POS POS POS" to stdout

```bash
$ cat {dataset} | rt.out | Tagger.out {learned.data} > tagged.data
```

- For the ConfusionMatrix utility: It expects a file name where the correct answers will be and will read the predicted data from stdin.

```bash
cat tagged.data | ./bcm.out {trainset}
```
