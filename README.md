<img src="inst/img/DiagrammeR.png">

[![Travis-CI Build Status](https://travis-ci.org/rich-iannone/DiagrammeR.svg?branch=master)](https://travis-ci.org/rich-iannone/DiagrammeR)
![](http://cranlogs.r-pkg.org/badges/grand-total/DiagrammeR?color=brightgreen)
[![Issue Stats](http://issuestats.com/github/rich-iannone/DiagrammeR/badge/pr?style=flat)](http://issuestats.com/github/rich-iannone/DiagrammeR)
[![Issue Stats](http://issuestats.com/github/rich-iannone/DiagrammeR/badge/issue?style=flat)](http://issuestats.com/github/rich-iannone/DiagrammeR)
[![codecov.io](https://codecov.io/github/rich-iannone/DiagrammeR/coverage.svg?branch=master)](https://codecov.io/github/rich-iannone/DiagrammeR?branch=master) 

With **DiagrammeR**, you can easily create graph diagrams. You can either use **Markdown**-like text to describe and render a diagram, or, use a collection of functions to create graph objects. The output can be viewed in the **RStudio** Viewer, it can be incorporated in **R Markdown**, and it can be integrated in **shiny** web apps. Because we are doing this in **R** we can and always should add much more **R** code into the mix.

Go to the [**project website**](http://rich-iannone.github.io/DiagrammeR/) and view a video walkthrough for a graph diagram that's created with a few lines of text and is just as easily customizable. After being all fired up from that intense video-tutorial extravaganza, have a look at the [**DiagrammeR Docs**](http://rich-iannone.github.io/DiagrammeR/docs.html) to learn more.

<img src="inst/img/simple-graph.png">

It's possible to make the above graph diagram using using **Graphviz** **DOT** code (as text within the **DiagrammeR** `grViz()` function) or through a combination of **DiagrammeR** functions, strung together with the **magrittr** `%>%` pipe. 

So, with **Graphviz**:

```r
library(DiagrammeR)

grViz("
digraph DAG {
      
  # Intialization of graph attributes
  graph [overlap = true]
      
  # Initialization of node attributes
  node [shape = box,
        fontname = Helvetica,
        color = blue,
        type = box,
        fixedsize = true]
      
  # Initialization of edge attributes
  edge [color = green,
        rel = yields]
      
  # Node statements
  1; 2; 3; 4; 8; 9; 10; 11
      
  # Revision to node attributes
  node [shape = circle]
      
  # Node statements
  5; 6; 7
      
  # Edge statements
  1->5; 2->6; 3->9; 4->7; 5->8; 5->10; 7->11

  # Revision to edge attributes
  edge [color = red]

  # Edge statements
  1->8; 3->6; 3->11; 3->7; 5->9; 6->10
}
")
```

With **magrittr** and **DiagrammeR**'s graph functions:

```r
library(DiagrammeR)
library(magrittr)

graph <-
  create_graph() %>%
  set_graph_name("DAG") %>%
  set_global_graph_attr("graph", "overlap", "true") %>%
  set_global_graph_attr("graph", "fixedsize", "true") %>%
  set_global_graph_attr("node", "color", "blue") %>%
  set_global_graph_attr("node", "fontname", "Helvetica") %>%
  add_n_nodes(11) %>%
  select_nodes_by_id(1:4) %>% 
  set_node_attr_with_selection("shape", "box") %>%
  set_node_attr_with_selection("type", "box") %>%
  clear_selection %>%
  select_nodes_by_id(5:7) %>% 
  set_node_attr_with_selection("shape", "circle") %>%
  set_node_attr_with_selection("type", "circle") %>%
  clear_selection %>%
  select_nodes_by_id(8:11) %>% 
  set_node_attr_with_selection("shape", "box") %>%
  set_node_attr_with_selection("type", "box") %>%
  clear_selection %>%
  add_edge(1, 5) %>% add_edge(2, 6) %>%
  add_edge(3, 9) %>% add_edge(4, 7) %>%
  add_edge(5, 8) %>% add_edge(5, 10) %>%
  add_edge(7, 11) %>% select_edges %>%
  set_edge_attr_with_selection("color", "green") %>%
  add_edge(1, 8) %>% add_edge(3, 6) %>%
  add_edge(3, 11) %>% add_edge(3, 7) %>%
  add_edge(5, 9) %>% add_edge(6, 10) %>%
  select_edges("color", "^$") %>%
  set_edge_attr_with_selection("color", "red") %>%
  clear_selection

render_graph(graph)
```

The graph functions allow you create graph objects, render those graphs, modify those graphs, get information from the graphs, create a series of graphs, perform scaling of attribute values with data values, and do other useful things.

This functionality makes it possible to generate a network graph with data available in tabular datasets. The general idea is to build specialized data frames that contain either node data and attributes (node data frames) and those data frames that contain edge data and edge attributes (edge data frames). These data frames are permitted to have node and edge attributes and also columns of other data. Because the attributes are always kept alongside the node and edge definitions (within the graph object itself), we can easily work with them and modify the values of the styling attributes and differentiate nodes and edges by size, color, shape, opacity, length, etc. Here is a listing of the available graph functions:

<img src="inst/img/graph_functions_1.png">
<img src="inst/img/graph_functions_2.png">
<img src="inst/img/graph_functions_3.png">
<img src="inst/img/graph_functions_4.png">
<img src="inst/img/graph_functions_5.png">
<img src="inst/img/graph_functions_6.png">
<img src="inst/img/graph_functions_7.png">
<img src="inst/img/graph_functions_8.png">

## Installation

**DiagrammeR** is used in an **R** environment. If you don't have an **R** installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](http://cran.rstudio.com). It is recommended that [**RStudio**](http://www.rstudio.com/products/RStudio/) be used as the **R** IDE to take advantage of its rendering capabilities and the code-coloring support for **Graphviz** and **mermaid** diagrams.

You can install the development version of **DiagrammeR** from **GitHub** using the **devtools** package.

```r
devtools::install_github('rich-iannone/DiagrammeR')
```

Or, get the v0.8.1 release from **CRAN**.

```r
install.packages('DiagrammeR')
```
