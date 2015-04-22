##
## Process Content Mine output XML files into node/edge lists that can be visualised in network graphs.
##
## The JSON output from this code can be fed into the visualiser here: https://github.com/matthewgthomas/ami-viz
##
## By: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
## Before running this code, run the following Content Mine pipeline:
## 1. `getpapers` to download articles
## 2. `norma` to create scholarly.html for all articles
## 3. `ami2-species` or `ami2-word` to generate the results.xml files
##
## The data directory (data.dir variable) should contain one subfolder for each article, named after the article's ID.
## After processing with AMI, these subfolders should contain a 'results' folder, which will in turn contain:
## - species/binomial/results.xml
## - word/frequencies/results.xml
## 
## The code here will take these files, summarise them across all articles and output JSON files ready to be visualised
## in a network graph (for example: https://github.com/matthewgthomas/ami-viz)
##
## 
## Copyright (c) 2015 Matthew Gwynfryn Thomas
## 
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## ## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##   
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
## 
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
##

#install.packages("XML", "RJSONIO")
require(XML)
require(RJSONIO)

# load functions
source("funcs-common.r")    # functions for loading, processing and outputting Content Mined data
source("funcs-words.r")     # for processing bag of words
source("funcs-species.r")  # for processing species lists

# Content Mine directory (desired structure described in header comments)
data.dir = "path/to/papers"
out.dir = "ami-viz/data"  # where to write the final JSON file(s)

top.x.words = 20  # keep only the top X most frequent words

# import data about downloaded papers
nodes = load_papers(data.dir)

# get a list of all .xml files in all subdirectories of data.dir
xml.files = list.files(data.dir, pattern="*xml", recursive=T)


#########################################################
## Word frequencies from bag of words
##
# keep only files with particular words in their directory structure
# - those directories correspond to the results from AMI
word.files = xml.files[ grep("*frequencies*", xml.files) ]  # bag of words

# get all bag of words results 
word.freqs = get_results(data.dir, word.files, nodes$id)

# keep only relevant data and clean up dataframe column formats
word.freqs = subset(word.freqs, select=c("article", "word", "count"))
word.freqs$word = as.character(word.freqs$word)  # returned as a list by default
word.freqs$count = as.integer(word.freqs$count)  # a list, by default

# calculate top ten word frequencies and pull out keywords (as nodes) and the papers they connect to (edges)
freq.list = calc_word_freq(word.freqs, top.x.words)
words.10 = freq.list[[1]]
edges.words = freq.list[[2]]

# clean up the nodes list, putting keywords and articles together - ready for output
nodes.words = finalise_nodes(words.10, nodes)

# save as a JSON file for the visualiser
output_json(nodes.words, edges.words, "words.json", out.dir)

#rm(nodes.words, edges.words)
rm(words.10, freq.list)


#########################################################
## Calculate word frequencies using term frequency - inverse document frequency (TF-IDF) method
##
freq.list = calc_word_freq_tfidf(word.freqs, top.x.words)
words.tfidf = freq.list[[1]]
edges.words = freq.list[[2]]

# clean up the nodes list, putting keywords and articles together - ready for output
nodes.words = finalise_nodes(words.tfidf, nodes)

# save as a JSON file for the visualiser
output_json(nodes.words, edges.words, "words_tdidf.json", out.dir)


#########################################################
## Calculate occurrences of species names in each of the articles
##
# keep only files with particular words in their directory structure
# - those directories correspond to the results from AMI
species.files = xml.files[ grep("*binomial*", xml.files) ]  # species lists
species.freqs = get_results(data.dir, species.files, nodes$id)

# clean up the results
species.freqs = subset(species.freqs, select=c("article", "match"))
species.freqs$match = as.character(species.freqs$match)

# calculate species occurrences and pull out species names (as nodes) and the papers they connect to (edges)
freq.list = calc_species(species.freqs)
nodes.species = freq.list[[1]]
edges.species = freq.list[[2]]

# clean up the nodes list, putting keywords and articles together - ready for output
nodes.species = finalise_nodes(nodes.species, nodes)

# save
output_json(nodes.species, edges.species, "species.json", out.dir)

# cleanup
rm(species.freqs, freq.list)
