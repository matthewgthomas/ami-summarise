##
## Process Content Mine output XML files into node/edge lists that can be visualised in network graphs.
##
## This file contains bag of words frequency functions.
##
## By: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
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

#install.packages("plyr")

# list of things to remove from word lists
to_remove = c(".±.", "[],", "[].", "(.)", ".%)", "(.%,", "(.–.)", "(−.%,", "(.,", "−.%", "():–.",
              "al.,", "al.", "Figure", "using", "used", "data", "results", "study", "studies")

#######################################
## Calculate frequencies of most common words
## 
## Params:
## - word.freqs: data frame of word frequencies -- output from get_results()
## - num.words: how many of the X most frequent words to return (default 10)
## 
## Returns: list containing 2 data frames
##          1. nodes - keywords and their total frequencies
##          2. edges - links between articles and keywords, with frequency of keyword in article
##
calc_word_freq = function(word.freqs, num.words=10) {
  library(plyr)
  
  words.totals = ddply(word.freqs, .(word), summarise, total=sum(count))
  
  # get rid of some nonsense
  words.totals = subset(words.totals, !word %in% to_remove)
  
  # get top X most frequent words
  words.totals = words.totals[ order(-words.totals$total), ]
  words.nodes = words.totals[1:num.words, ]
  
  # get papers containing these top X words. This will be the edge list
  words.edges = subset(word.freqs, word %in% words.nodes$word)
  
  # clean up edges dataframe
  row.names(words.edges) = NULL  # get rid of row names
  names(words.edges) = c("source", "target", "count")  # rename columns
  
  return(list(words.nodes, words.edges))
}


#######################################
## Calculate frequencies of most common words using term frequency–inverse document frequency (TF-IDF)
## 
## Params:
## - word.freqs: data frame of word frequencies -- output from get_results()
## - num.words: how many of the X most frequent words to return (default 10)
## 
## Returns: list containing 2 data frames
##          1. nodes - keywords and their total frequencies
##          2. edges - links between articles and keywords, with frequency of keyword in article
##
calc_word_freq_tfidf = function(word.freqs, num.words=10) {
  library(plyr)
  
  # calculate the total number of times each word appears across articles
  word.counts = ddply(subset(word.freqs, select=c(word)), .(word), count)
  # calculate the total number of words in each article
  article.TF = ddply(subset(word.freqs, select=c(article)), .(article), count)
  
  # term frequency (TF) for term t = no. times t appears in an article / no. terms in the article
  word.freqs = merge(word.freqs, article.TF, by="article", all.x=T)
  word.freqs$TF = word.freqs$count / word.freqs$freq
  
  # inverse document frequency (IDF) for term t = log(no. articles / no. articles containing t)
  N = length(unique(word.freqs$article))  # number of articles
  word.counts$IDF = log2(N / word.counts$freq)
  
  # merge the IDFs into the word frequency dataframe
  word.freqs = merge(word.freqs, word.counts, by="word", all.x=T)
  
  # calculate TF-IDF by multiplying TF by the IDF
  word.freqs$TFIDF = word.freqs$TF * word.freqs$IDF
  
  # get rid of some nonsense
  word.freqs = subset(word.freqs, !word %in% to_remove)
  
  # get top X most frequent words, sorted by TF-IDF
  words.totals = subset(word.freqs, select=c(word, TFIDF))
  words.totals = unique(words.totals[ order(-words.totals$TFIDF), ])
  words.nodes = words.totals[1:num.words, ]
  
  # get papers containing these top X words. This will be the edge list
  words.edges = subset(word.freqs, word %in% words.nodes$word, select=c(word, article, TFIDF))
  
  # clean up edges dataframe
  row.names(words.edges) = NULL  # get rid of row names
  names(words.edges) = c("source", "target", "count")  # rename columns
  
  return(list(words.nodes, words.edges))
}
