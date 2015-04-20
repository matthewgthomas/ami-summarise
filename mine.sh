#!/bin/sh

getpapers --query '"search" AND "terms"' --outdir ~/papers

# normalise all downloaded files by looping over subdirectories separately
for subdir in *; do norma -q $subdir -i fulltext.html -o scholarly.html --xsl nlm2html; done;

# bag of words
for subdir in *; do ami2-word -q $subdir -i scholarly.html --context 35 50 --w.words wordFrequencies --w.stopwords /org/xmlcml/ami2/plugins/word/stopwords.txt; done;
# species
for subdir in *; do ami2-species -q $subdir -i scholarly.html --sp.species --context 35 50 --sp.type binomial genus genussp; done;
