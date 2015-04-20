##
## Process Content Mine output XML files into node/edge lists that can be visualised in network graphs.
##
## This file contains the species summariser function.
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

#######################################
## Summarise species occurrences
##
calc_species = function(species.freqs) {
  # add up number of occurrences by species name and article
  edges.species = ddply(species.freqs, .(match), count)
  # sort out edge list columns
  names(edges.species) = c("source", "target", "count")
  
  # count up total occurrences of each unique species name
  all.species = ddply(edges.species, .(target), summarise, n = sum(count))
  
  return(list(all.species, edges.species))
}
