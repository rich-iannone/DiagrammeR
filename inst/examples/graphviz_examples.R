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

V = letters
randomEGraph(V, edges=30) %>>%
  toDotR(tf)
readLines(tf) %>>%
  grViz(engine="circo")
unlink(tf)


# this is fun


# some examples from the graphviz gallery
#  these really don't work in RStudio viewer
d = readLines("http://www.graphviz.org/Gallery/directed/fsm.gv.txt") 
d %>>% grViz
d %>>% grViz(engine="neato")
d %>>% grViz(engine="twopi")
d %>>% grViz(engine="circo")

d = readLines("http://www.graphviz.org/Gallery/directed/Genetic_Programming.gv.txt")
d %>>% grViz
d %>>% grViz(engine="neato")
d %>>% grViz(engine="twopi")
d %>>% grViz(engine="circo")


readLines("http://www.graphviz.org/Gallery/directed/unix.gv.txt") %>>%
  grViz




# test single quote  in spec
grViz("
  digraph G {
    node [shape='plaintext']
    a [label=<<TABLE BORDER='0' CELLBORDER='1' CELLSPACING='0'>
         <TR><TD PORT='1'>one<TD PORT='2' ROWSPAN='2'>two
       <TR><TD PORT='3'>three
       >];
    b [label=<<TABLE BORDER='0' CELLBORDER='1' CELLSPACING='0'>
         <TR><TD PORT='1'>one<TD PORT='2'>two
       <TR><TD PORT='3' COLSPAN='2'>three
       >];
    a:1 -> b:2;
    b:1 -> a:2;
    b:3 -> a:3;
  }
")


# one more from neato user manual
#   http://www.graphviz.org/pdf/neatoguide.pdf
grViz('
graph G {
node [shape=box,style=filled];
{node [width=.3,height=.3,shape=octagon,style=filled,color=skyblue] A1 A2 A3}
A -- A1 [label="l #6"];
      A -- A2 [label="l #7"];
      A -- A3 [label="l #8"];
{edge [style=invis]; A1 -- A2 -- A3}
edge [len=3]; /* applies to all following edges */
  A -- B [label="l #1"]; A -- C [label="l #2"]; A -- D [label="l #3"];
A -- E [label="l #4"]; A -- F [label="l #5"]; B -- C [label="l #1"];
B -- E [label="l #2"]; B -- F [label="l #3"]; C -- D [label="l #1"];
D -- E [label="l #1"];
}
', engine = "neato")


# and a twopi example from the gallery
#   http://www.graphviz.org/Gallery/twopi/twopi2.gv.txt

readLines("http://www.graphviz.org/Gallery/twopi/twopi2.gv.txt") %>>%
  grViz(engine="twopi",height=2000,width=2000)
