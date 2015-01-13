# unfortunately they will not render properly in RStudio on Windows
#  we have run into this problem with some rCharts libraries
#  and I cannot figure out why they just do not show up

# nevertheless, here are some examples of the crude binding for viz.js

#devtools::install("timelyportfolio/DiagrammeR@feature-graphviz")
library(DiagrammeR)
library(rvest)
library(XML)
library(pipeR)

# think you are on Mac but if on Windows do this first
# options(viewer=NULL)

# do all the examples from viz.js
html("https://raw.githubusercontent.com/mdaines/viz.js/gh-pages/example.html") %>>%
  html_nodes("script[type='text/vnd.graphviz']") %>>%
  lapply(
    function(x){
      xmlValue(x) %>>% (~ htmltools::html_print(grViz(.)) ) %>>% DiagrammeR(type="grViz")
    }
  )


# now using Rgraphviz as another experiment
library(Rgraphviz)
example(randomGraph)
tf <- tempfile()

g1 %>>% toDotR(tf)
readLines(tf) %>>%
  grViz
unlink(tf)


# this is fun


# some examples from the graphviz gallery
#  these really don't work in RStudio viewer
readLines("http://www.graphviz.org/Gallery/directed/fsm.gv.txt") %>>%
  grViz

readLines("http://www.graphviz.org/Gallery/directed/Genetic_Programming.gv.txt") %>>%
  grViz

readLines("http://www.graphviz.org/Gallery/directed/unix.gv.txt") %>>%
  grViz
