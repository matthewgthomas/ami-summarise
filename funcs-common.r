##
## Process Content Mine output XML files into node/edge lists that can be visualised in network graphs.
##
## This file contains the main input, processing and output functions.
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

#install.packages("plotrix", "RJSONIO", "XML")

#########################################################
## Create list of nodes for the network viz.
## This function takes the JSON file produced by `getpapers` and outputs a dataframe of articles.
##
## Params:
## - data.dir : the Content Mine directory containing the JSON file (and everything else)
## - filename : the JSON file containing details of all scraped/processed papers
##
## Returns: dataframe containing details of the articles in the form of a node list, with these columns:
##          - id   : article's ID, corresponding to subdirectories in data.dir (one subdir for every article)
##          - name : article's title
##          - url  : article's DOI
##          - type : 2=article; 1=keyword/species etc. (type 1 will come into play a bit later)
##          - size : the node's radius -- set to the same value for all articles (will be modified later)
##
load_papers = function(data.dir, filename="all_results.json") {
  library(RJSONIO)
  
  # load JSON file containing results in this directory
  papers = fromJSON(file.path(data.dir, filename))
  
  # set up blank dataframe to hold all the papers
  nodes = data.frame(id=rep("", length(papers)), title=rep("", length(papers)), doi=rep("", length(papers)), stringsAsFactors=F)
  
  # loop each article in the json file and add to node list
  for (i in 1:length(papers)) {
    pmcid = ifelse(is.null(papers[[i]]$pmcid), "", papers[[i]]$pmcid)
    title = ifelse(is.null(papers[[i]]$title), "", papers[[i]]$title)
    doi = ifelse(is.null(papers[[i]]$DOI), "", papers[[i]]$DOI)
    
    nodes[i,] = c(pmcid, title, doi)
  }
  
  # clean up the dataframe of papers and put it in the correct format to be the list of nodes
  nodes$type = 2  # default for articles
  nodes$size = 50 # default for articles
  row.names(nodes) = NULL  # get rid of row names
  names(nodes) = c("id", "name", "url", "type", "size")  # rename columns
  
  return(nodes)
}


#########################################################
## Load XML results files created by AMI
##
## Params:
## - data.dir    : base of the Content Mine directory to process
## - xml.files   : list of .xml files to load 
## - article_ids : vector of article ID numbers (found in nodes$id created by load_papers()) -- these must correspond to sub-directories in data.dir
##
## Returns: dataframe containing all results from everything in the xml.files list
##
get_results = function(data.dir, xml.files, article_ids) {
  library(XML)
  
  # open all .xml files in data directory and save them as separate variables in the workspace
  for (i in 1:length(xml.files)) assign(strsplit(xml.files[i], "/")[[1]][1],  # strsplit gets the publication ID, which is the first part of the firectory name
                                        xmlRoot(xmlTreeParse(file.path(data.dir, xml.files[i]), useInternalNode = TRUE)))
  
  # function to extract nodes and attributes from xml
  # code from: http://stackoverflow.com/a/16805244/951174
  dumFun <- function(x) {
    xname <- xmlName(x)
    xattrs <- xmlAttrs(x)
    c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)
  }
  
  xml.path = "//*/result"  # this should be the same for all AMI output .xml files
  
  # place to hold all the results from all articles
  results = data.frame()
  
  # loop all xml data, storing everything in the results data frame
  for (xml.data.name in article_ids) {
    tmp_xml = try(get(xml.data.name), silent=T)  # grab the data
    
    if (class(tmp_xml)[1] != "try-error") {  # ignore errors -- they're most likely empty or missing .xml files
      # convert the results nodes to a dataframe
      tmp_df = as.data.frame(t(xpathSApply(tmp_xml, xml.path, dumFun)), stringsAsFactors = FALSE)
      
      if (ncol(tmp_df) > 0) {  # check the data aren't missing/malformed
        tmp_df$article = xml.data.name  # add the article's name
        
        # put results in collected dataframe
        results = rbind(results, tmp_df)
      }
    }
  }
  
  return(results)
}


#########################################################
## Create list of nodes for the network viz.
## This function takes the JSON file produced by `getpapers` and outputs a dataframe of articles.
##
## Params:
## - nodes.freq     : keywords/species/etc. dataframe output by calc_word_freq() or calc_species()
## - nodes.articles : nodes dataframe containing all articles (ouput from load_papers())
##
## Returns: dataframe containing all keywords/species/etc. and all articles in a node list
##
finalise_nodes = function(nodes.freq, nodes.articles) {
  library(plotrix)
  
  # set this variable to change the size of article nodes relative to keyword/species nodes
  # = 1 means that article nodes will be the same size as the smallest keyword/species
  relative_size_smallest_node = 1  # larger numbers mean smaller article nodes relative to smallest keyword/species
  
  # add keywords in 'nodes.freq' to nodesa.articles list of papers
  names(nodes.freq) = c("id", "size")
  nodes.freq$name = nodes.freq$id
  nodes.freq$url = ""  # no url for keywords
  nodes.freq$type = 1  # for keywords
  
  # the main summariser functions all output wildly different ranges of node sizes -- rescale them
  nodes.freq$size = rescale(nodes.freq$size, range(10, 100))
  
  nodes.freq = nodes.freq[, c(1, 3, 4, 5, 2) ]  # reorder columns
  nodes.out = rbind(nodes.freq, nodes)  # prepend keywords to articles
  
  # scale article node sizes relative to the smallest keyword/species (but min. size = 1)
  min.words = max(min(nodes.freq$size / relative_size_smallest_node), 1)
  # set article node sizes to this minimum
  nodes.out$size = ifelse(nodes.out$type==2, min.words, nodes.out$size)
  
  return(nodes.out)
}


#######################################
## Output to JSON
##
output_json = function(nodes, edges, filename, output.dir = ".") {
  library(RJSONIO)
  
  # put nodes and links together for json output
  # code: http://stackoverflow.com/a/13505824/951174
  json_out <- list(
    nodes = unname(alply(nodes, 1, identity)),
    links = unname(alply(edges, 1, identity))
  )
  
  sink(file.path(output.dir, filename))
  cat(toJSON(json_out))
  sink()
  
  print(paste("Written", filename))
}
