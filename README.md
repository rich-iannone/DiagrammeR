<img src="inst/img/DiagrammeR.png">

With the **DiagrammeR** package, you can create graph diagrams and flowcharts using **R**. Markdown-like text is used to describe a diagram and, by doing this in **R**, we can also add some **R** code into the mix and integrate these diagrams in the **R** console, through **R Markdown**, and in **shiny** apps. 

Go to the [**project website**](http://rich-iannone.github.io/DiagrammeR/) and view a video walkthrough for a graph diagram that's created with a few lines of text and is just as easily customizable.

The package leverages the infrastructure provided by [**htmlwidgets**](http://htmlwidgets.org) to bridge **R** and  both [**mermaid.js**](https://github.com/knsv/mermaid) and [**viz.js**](https://github.com/mdaines/viz.js/).

<img src="inst/img/DiagrammeR_flow_diagram.png">

### Installation

Install the development version of **DiagrammeR** from GitHub using the **devtools** package.

```R
devtools::install_github('rich-iannone/DiagrammeR')
```

Or, get the 0.4 release from CRAN.

```R
install.packages('DiagrammeR')
```

### Graphviz Graphs

It's possible to make diagrams using the **Graphviz** support included in the **DiagrammeR** package. The processing function is called `grViz`. What you pass into `grViz` is a valid graph in the **DOT** language. The text can either exist in the form of a string, a reference to a **Graphviz** file (with a **.gv** file extension), or as a text connection.

#### Defining a Graphviz Graph

The **Graphviz** graph specification must begin with a directive stating whether a directed graph (`digraph`) or an undirected graph (`graph`) is desired. Semantically, this indicates whether or not there is a natural direction from one of the edge's nodes to the other. An optional graph `ID` follows this and paired curly braces denotes the body of the statement list (`stmt_list`). 

Optionally, A graph may also be described as `strict`. This forbids the creation of multi-edges, i.e., there can be at most one edge with a given tail node and head node in the directed case. For undirected graphs, there can be at most one edge connected to the same two nodes. Subsequent edge statements using the same two nodes will identify the edge with the previously defined one and apply any attributes given in the edge statement.

Here is the basic structure:

`[strict] (graph | digraph) [ID] '{' stmt_list '}'`

#### Statements

The graph statement (`graph_stmt`), the node statement (`node_stmt`), and the edge statement (`edge_stmt`) are the three most commonly used statements in the **Graphviz** **DOT** language. Graph statements allow for attributes to be set for all components of the graph. Node statements define and provide attributes for graph nodes. Edge statements specify the edge operations between nodes and they supply attributes to the edges. For the edge operations, a directed graph must specify an edge using the edge operator `->` while an undirected graph must use the `--` operator.

Within these statements follow statement lists. Thus for a node statement, a list of nodes is expected. For an edge statement, a list of edge operations. Any of the list item can optionally have an attribute list (`attr_list`) which modify the attributes of either the node or edge.

Comments may be placed within the statement list. These can be marked using `//` or a `/* */` structure. Comment lines are denoted by a `#` character. Multiple statements within a statement list can be separated by linebreaks or `;` characters between multiple statements.

Here is an example where nodes (in this case styled as boxes and circles) can be easily defined along with their connections:

```R
boxes_and_circles <- "
digraph boxes_and_circles {
  
  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
    A; B; C; D; E; F
  
  node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
    1; 2; 3; 4; 5; 6; 7; 8

  # several 'edge' statements
    A->1; B->2; B->3; B->4; C->A
    1->D; E->A; 2->4; 1->5; 1->F
    E->6; 4->6; 5->7; 6->7; 3->8

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]
}
"

grViz(boxes_and_circles)
```

<img src="inst/img/grViz_1.png">

The attributes of the nodes and the edges can be easily modified. In the following, colors can be selectively changed in attribute lists.

```R
boxes_and_circles <- "
digraph boxes_and_circles {
  
  # several 'node' statements
  node [shape = box,
        fontname = Helvetica,
        color = blue] // for the letter nodes, use box shapes
    A; B; C; D; E
    F [color = black]
  
  node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
    1; 2; 3; 4; 5; 6; 7; 8

  # several 'edge' statements
  edge [color = gray] // this sets all edges to be gray (unless overridden)
    A->1; B->2
    B->3 [color = red]
    B->4
    C->A [color = green]
    1->D; E->A; 2->4; 1->5; 1->F
    E->6; 4->6; 5->7; 6->7
    3->8 [color = blue]

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]
}
"

grViz(boxes_and_circles)
```

<img src="inst/img/grViz_2.png">

There are many more attributes. Here are the principal node attributes:

|Node Attribute| Description                                                 | Default          |
|:-------------|:------------------------------------------------------------|:-----------------|
|`color`       | the node shape color                                        | `black`          |
|`colorscheme` | the scheme for interpreting color names                     |                  |
|`distortion`  | node distortion for any `shape = polygon`                   |`0.0`             |
|`fillcolor`   | node fill color                                             |`lightgrey/black` |
|`fixedsize`   | label text has no affect on node size                       |`false`           |
|`fontcolor`   | the font color                                              |`black`           |
|`fontname`    | the font family                                             |`Times-Roman`     |
|`fontsize`    | the point size of the label                                 |`14`              |
|`group`       | the name of the node's horizontal alignment group           |                  |
|`height`      | the minimum height in inches                                |`0.5`             |
|`image`       | the image file name                                         |                  |
|`labelloc`    | the node label vertical alignment                           |`c`               |
|`margin`      | the space around a label                                    |`0.11, 0.55`      |
|`orientation` | the node rotation angle                                     |`0.0`             |
|`penwidth`    | the width of the pen (in point size) for drawing boundaries |`1.0`             |
|`peripheries` | the number of node boundaries                               |                  |
|`shape`       | the shape of the node                                       |`ellipse`         |
|`sides`       | the number of sides for `shape = polygon`                   |`4`               |
|`skew`        | the skewing of the node for `shape = polygon`               |`0.0`             |
|`style`       | graphics options for the node                               |                  |
|`tooltip`     | the tooltip annotation for the node                         |[*node label*]    |
|`width`       | the minimum width in inches                                 |`0.75`            |

The edge attributes:

|Edge Attribute    | Description                                                  | Default         |
|:-----------------|:-------------------------------------------------------------|:----------------|
|`arrowhead`       | style of arrowhead at head end                               | normal          |
|`arrowsize`       | scaling factor for arrowheads                                | `1.0`           |
|`arrowtail`       | style of arrowhead at tail end                               | normal          |
|`color`           | edge stroke color                                            | `black`         |
|`colorscheme`     | the scheme for interpreting color names                      |                 |
|`constraint`      | whether edge should affect node ranking                      | true            |
|`decorate`        | setting this draws line between labels with their edges      |                 | 
|`dir`             | direction; either `forward`, `back`, `both`, or `none`       | `forward`       |
|`edgeURL`         | URL attached to non-label part of edge                       |                 |
|`edgehref`        | same as `edgeURL` attribute                                  |                 |
|`edgetarget`      | if an URL is set, this determines the browser window for URL |                 |
|`edgetooltip`     | a tooltip annotation for the non-label part of edge          | label           |
|`fontcolor`       | the font color                                               | `black`         |
|`fontname`        | the font family                                              | `Times-Roman`   |
|`fontsize`        | the point size of the label                                  | `14`            |
|`headclip`        | if false, edge is not clipped to head node boundary          | true            |
|`headhref`        | same as `headURL`                                            |                 |
|`headlabel`       | label placed near head of edge                               |                 |
|`headport`        | can be either: `n`, `ne`, `e`, `se`, `s`, `sw`, `w`, `nw`    |                 |
|`headtarget`      | if `headURL` is set, determines the browser window for URL   |                 |
|`headtooltip`     | a tooltip annotation near head of edge                       | label           |
|`headURL`         | URL attached to head label                                   |                 |
|`href`            | alias for URL                                                |                 |
|`id`              | any string (user-defined output object tags)                 |                 |
|`label`           | edge label                                                   |                 |
|`labelangle`      | angle in degrees which head or tail label is rotated off edge| `-25.0`         |
|`labeldistance`   | scaling factor for distance of head or tail label from node  | `1.0`           |
|`labelfloat`      | lessen constraints on edge label placement                   | false           |
|`labelfontcolor`  | typeface color for head and tail labels                      | `black`         |
|`labelfontname`   | font family for head and tail labels                         | `Times-Roman`   |
|`labelfontsize`   | point size for head and tail labels                          | `14`            |
|`labelhref`       | same as `labelURL`                                           |                 |
|`labelURL`        | URL for label, overrides `edgeURL`                           |                 |
|`labeltarget`     | if `URL` or `labelURL` set, determines browser window for URL|                 |
|`labeltooltip`    | tooltip annotation near label                                | label           |
|`layer`           | `all`, *id* or *id*:*id*, or a comma-separated list          | overlay range   |
|`lhead`           | name of cluster to use as head of edge                       |                 |
|`ltail`           | name of cluster to use as tail of edge                       |                 |
|`minlen`          | minimum rank distance between head and tail                  | `1`             |
|`penwidth`        | width of pen for drawing edge stroke, in points              | `1.0`           |
|`samehead`        | tag for head node; edge heads with the same tag are merged onto the same port ||
|`sametail`        | tag for tail node; edge tails with the same tag are merged onto the same port ||
|`style`           | graphics options                                             |                 |
|`tailclip`        | if false, edge is not clipped to tail node boundary          | true            |
|`tailhref`        | same as `tailURL`                                            |                 |
|`taillabel`       | label placed near tail of edge                               |                 |
|`tailport`        | can be either: `n`, `ne`, `e`, `se`, `s`, `sw`, `w`, `nw`    |                 |
|`tailtarget`      | if `tailURL` is set, determines browser window for URL       |                 |
|`tailtooltip`     | tooltip annotation near tail of edge                         | label           |
|`tailURL`         | URL attached to tail label                                   |                 |
|`target`          | if `URL` is set, determines browser window for URL           |                 |
|`tooltip`         | tooltip annotation                                           | label           |
|`weight`          | integer cost of stretching an edge                           | `1`             |

The graph attributes:

|Graph Attribute| Description                                                  | Default          |
|:--------------|:-------------------------------------------------------------|:-----------------|
|`aspect`       | controls aspect ratio adjustment                             |                  |
|`bgcolor`      | background color for drawing and initial fill color          |                  |
|`center`       | center drawing                                               | false            |
|`clusterrank`  | `local` but optionally `global` or `none`                    | `local`          |
|`color`        | the color for clusters, outline color, and fill color        | `black`          |
|`colorscheme`  | the scheme for interpreting color names                      |                  |
|`compound`     | allow edges between clusters                                 | false            |
|`concentrate`  | enables edge concentrators                                   | false            |
|`dpi`          | dpi for image output                                         | 96               |
|`fillcolor`    | cluster fill color                                           | `black`          |
|`fontcolor`    | typeface color                                               | `black`          |
|`fontname`     | font family                                                  | `Times-Roman`    |
|`fontpath`     | list of directories to search for paths                      |                  |
|`fontsize`     | point size of label                                          | `14`             |
|`id`           | any string (user-defined output object tags)                 |                  |
|`label`        | any string                                                   |                  |
|`labeljust`    | label justification; `l` or `r` for left or right            | centered         |
|`labelloc`     | label location; `t` or `b` for top or bottom                 | top              |
|`landscape`    | graph orientation; `true` for landscape                      |                  |
|`layers`       | *id*:*id*:*id*...                                            |                  |
|`layersep`     | specifies separator character to split `layers`              | `:`              |
|`margin`       | margin (in inches) included in `page`                        | `0.5`            |
|`mindist`      | minimum separation (in inches) between all nodes             | `1.0`            |
|`nodesep`      | separation (in inches) between nodes                         | `0.25`           |
|`nojustify`    | justify to label if set as true                              | false            |
|`ordering`     | if `out` edge order is preserved                             |                  |
|`orientation`  | if `rotate` is not used and the value is `landscape`, then landscape | `portrait` |
|`outputorder`  | or `nodesfirst`, `edgesfirst`                                | breadthfirst     |
|`page`         | unit of pagination (e.g., "`8.5,11`")                        |                  |
|`pagedir`      | traversal order of pages                                     | `BL`             |
|`pencolor`     | color for drawing cluster boundaries                         | `black`          |
|`penwidth`     | width of pen, in points, for drawing boundaries              | `1.0`            |
|`peripheries`  | number of cluster boundaries                                 | `1`              |
|`rank`         | choices are: `same`, `min`, `max`, `source` or `sink`        |                  |
|`rankdir`      | choices are: `LR` (left to right) or `TB` (top to bottom)    | `TB`             |
|`ranksep`      | separation between ranks, in inches                          | `0.75`           |
|`ratio`        | approximate aspect ratio desired: `fill` or `auto`           |                  |
|`rotate`       | if set to `90`, set orientation to landscape                 |                  |
|`samplepoints` | number of points used to represent ellipses and circles on output | `8`         | 
|`searchsize`   | maximum edges with negative cut values to check when looking for a minimum one during network simplex| `30` |
|`size`         | maximum drawing size, in inches                              |                  |
|`splines`      | draw edges as splines, polylines, lines                      |                  |
|`style`        | graphics options for clusters (e.g., `filled`)               |                  |
|`stylesheet`   | pathname or URL to XML style sheet for SVG                   |                  |
|`target`       | if `URL` is set, determines browser window for URL           |                  |
|`tooltip`      | tooltip annotation for cluster                               | label            |
|`truecolor`    | if set, force 24-bit or indexed color in image output        |                  |
|`URL`          | URL associated with graph (format-dependent)                 |                  |
|`viewport`     | clipping window on output                                    |                  |

#### Graphviz Substitution

Inspired by Razor and the footnote URLs from Markdown, substitution allows for mixing in R expressions into a Graphviz graph specification without sacrificing readability. In the simple example of specifying a single node, the following substitution syntax would be used:

```
digraph {
@@1
}

[1]: 'a'
```

Importantly, the footnote expressions should reside below the closing curly brace of the `graph` or `digraph` expression. It should always take the form of:

`[` + *`[footnote number]`* + `]:`

In the above example, the `[1]:` footnote expression evaluates as `'a'`, and, that is what's substituted at the `@@1` location (where, in turn, it will be taken as the node ID). The substitution construction is:

`@@` + *`[footnote number]`*

Substitutions can also be used to insert values from vector indices into the graph specification. Simply use this format:

`@@` + *`[footnote number]`* + `-` + *`[index number]`*

Here is an example of substituting alphabet letters from **R**'s `LETTERS` constant into a Graphviz graph specification.

```
digraph {
alpha
@@1-1; @@1-2; @@1-3; @@1-4; @@1-5
@@1-6; @@1-7; @@1-8; @@1-9; @@1-10
}

[1]: LETTERS
```

After evaluation of the footnote expressions and substitution, the graph specification becomes this:

```
digraph {
alpha
A; B; C; D; E
F; G; H; I; J
}
```

To take advantage of substitution and render the graph, simply use the `grViz` function with the graph specification:

```R
grViz("...graph spec with substitutions...")
```

A mixture of both types of subtitutions can be used. As an example:

```R
grViz("
digraph a_nice_graph {

# node definitions with substituted label text
node [fontname = Helvetica]
a [label = '@@1']
b [label = '@@2-1']
c [label = '@@2-2']
d [label = '@@2-3']
e [label = '@@2-4']
f [label = '@@2-5']
g [label = '@@2-6']
h [label = '@@2-7']
i [label = '@@2-8']
j [label = '@@2-9']

# edge definitions with the node IDs
a -> {b c d e f g h i j}
}

[1]: 'top'
[2]: 10:20
")
```

As can be seen in the following output: (1) the node with ID `a` is given the label `top` (after substituting `@@1` with expression after the `[1]:` footnote expression), (2) the nodes with ID values from `b`-`j` are respectively provided values from indices 1 to 9 (using the hypenated form of `@@`) of the evaluated expression `10:20` (in the `[2]:` footnote expression).

<img src="inst/img/grViz_3.png">

Footnote expressions are meant to be flexible. They can span multiple lines, and they can also take in objects that are available in the global workspace. So long as a vector object results from evaluation, substitution can be performed.

Here is an example of a diagram created using R and RStudio information (obtained from the `R.Version` and `rstudio::versionInfo` functions):

```R
grViz("
digraph nicegraph {

  # graph, node, and edge definitions
  graph [compound = true, nodesep = .5, ranksep = .25,
         color = crimson]

  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 1,
        color = darkslategray]

  edge [color = grey, arrowhead = none, arrowtail = none]

  # subgraph for R information
  subgraph cluster0 {
    node [fixedsize = true, width = 3]
    '@@1-1' -> '@@1-2' -> '@@1-3' -> '@@1-4'
    '@@1-4' -> '@@1-5' -> '@@1-6' -> '@@1-7'
  }

  # subgraph for RStudio information
  subgraph cluster1 {
    node [fixedsize = true, width = 3]
    '@@2' -> '@@3'
  }

  Information             [width = 1.5]
  Information -> R
  Information -> RStudio
  R -> '@@1-1'            [lhead = cluster0]
  RStudio -> '@@2'        [lhead = cluster1]

}

[1]: paste0(names(R.Version())[1:7], ':\\n ', R.Version()[1:7])
[2]: paste0('RStudio version:\\n ', rstudio::versionInfo()[[1]])
[3]: paste0('Current program mode:\\n ', rstudio::versionInfo()[[2]])

")
```

The output will of course vary by the system on which it was generated. Here is my output:

<img src="inst/img/grViz_4.png">

#### Using Data Frames to Define Graphviz Graphs

The `graphviz_single_df` function is provided for generating a chunk of **Graphviz** **DOT** code (specifically `node` and `edge` statements) from a single data frame. The basic idea is to have a prepared data frame available, call the `graphviz_single_df` function to create an string object with the **DOT** code, and then use substitution in the `grViz` function call to insert that **DOT** code.

Edges can be defined between two different columns in the data frame by supplying a string in the form of `[dfcol_1] -> [dfcol_2]` for the `edge_between` argument.

Node attributes can be defined using the `node_attr` argument. Here, a string vector is to be provided using the construction: `c("[dfcol_1]: [node_attr_1] = [value], [node_attr_2] = [value], ...", "[dfcol_2]: [node_attr_1] = [value], [node_attr_2] = [value], ...")`.
 
Edge attributes can be provided as well. A string vector should be provided with the construction: `"1: [edge_attr_1] = [value], [edge_attr_2] = [value], ..."`. Additionally, edge attributes can be scaled to values in a data frame column. This is done by creating a statement in the form: `[edge_attr] [value_1] to [value_2] with [dfcol]`. Currently, scales can be generated from numeric and color values.

The following example outlines how a data frame (with some beforehand preparation) can be used to generate a graph diagram. 

```R
# Get unique pairs of flight origin and destination as a df
unique_routes <- unique(nycflights13::flights[,11:12])
 
# Add column with number of flights for each route
for (i in 1:nrow(unique_routes)){
  if (i == 1) add_column <- ncol(unique_routes) + 1
 
  unique_routes[i, add_column] <-
    nrow(subset(nycflights13::flights,
                origin == as.vector(unique_routes[i,1], mode = 'character') &
                  dest == as.vector(unique_routes[i,2], mode = 'character')))
 
  if (i == nrow(unique_routes)){
    colnames(unique_routes)[ncol(unique_routes)] <- 'flights'
  }
}
  
# Sort 'unique_routes' by descending number of flights
unique_routes <- unique_routes[with(unique_routes, order(-flights)), ]
rownames(unique_routes) <- NULL
 
# Use the 'graphviz_single_df' function to specify what to graph, using
# the 'unique_routes' data frame
nodes_edges <-
  graphviz_single_df(
    df = unique_routes,
    edge_between = c("origin -> dest"),
    node_attr = c("origin:
                   shape = circle,
                   style = filled,
                   height = 2,
                   layer = 'all',
                   fontname = Helvetica,
                   fontsize = 42,
                   fillcolor = lightblue",
                  "dest: 
                   shape = circle,
                   style = filled,
                   height = 1,
                   layer = 'all',
                   fontname = Helvetica,
                   fontsize = 0,
                   fillcolor = seagreen3"),
    edge_attr = "1:
                   color = #ff000040,
                   arrowhead = dot,
                   penwidth 2 to 50 with flights,
                   arrowsize 1 to 10 with flights,
                   color blue to red with flights
                  ")
 
# Create the graph by inserting the 'nodes_edges' object into
# the Graphviz DOT specification with the substitution syntax
grViz("
digraph flights {

  # Graph statements
  graph [layout = twopi,
         overlap = false,
         fixedsize = true,
         ranksep = 11,
         outputorder = edgesfirst]

  # Nodes and edges
  @@1

}
[1]: nodes_edges
")
```

<img src="inst/img/grViz_8.png">

#### Graphviz Engines

Several **Graphviz** engines are available with **DiagrammeR** for rendering graphs. By default, the `grViz` function renders graphs using the standard **dot** engine. However, the **neato**, **twopi**, and **circo** engines are selectable by supplying those names to the `engine` argument. The **neato** engine provides spring model layouts. This is a suitable engine if the graph is not too large (<100 nodes) and you don't know anything else about it. The **neato** engine attempts to minimize a global energy function, which is equivalent to statistical multi-dimensional scaling. The **twopi** engine provides radial layouts. Nodes are placed on concentric circles depending their distance from a given root node. The **circo** engine provide circular layouts. This is suitable for certain diagrams of multiple cyclic structures, such as certain telecommunications networks.

Here is how the 'boxes_and_circles' graph is rendered with the **neato**, **twopi**, and **circo** engines:

```R
grViz(boxes_and_circles, engine = "neato")
```

<img src="inst/img/grViz_5.png">

```R
grViz(boxes_and_circles, engine = "twopi")
```

<img src="inst/img/grViz_6.png">

```R
grViz(boxes_and_circles, engine = "circo")
```

<img src="inst/img/grViz_7.png">

### Mermaid Graphs

The `mermaid` function processes the specification of a diagram and then renders the diagram. This diagram spec can either exist in the form of a string, a reference to a mermaid file (with a **.mmd** file extension), or as a connection. 

The **mermaid**-style graph specification begins with a declaration of `graph` followed by the graph direction. The directions can be:

- `LR` left to right
- `RL` right to left
- `TB` top to bottom
- `BT` bottom to top
- `TD` top down (same as `TB`)

Nodes can be given arbitrary ID values and those IDs are displayed as text within their respective boxes. Connections between nodes are denoted by:

- `-->` arrow connection
- `---` line connection

Simply joining up a series of nodes in a left-to-right graph can be done in a few lines:

```R
diagram <- "
graph LR
  A-->B
  A-->C
  C-->E
  B-->D
  C-->D
  D-->F
  E-->F
"

mermaid(diagram)
```

This renders the following image:

<img src="inst/img/mermaid_1.png">

The same result can be achieved in a more succinct manner with this **R** statement (using semicolons between statements in the **mermaid** diagram spec):

```R
mermaid("graph LR; A-->B; A-->C; C-->E; B-->D; C-->D; D-->F; E-->F")
```

Alternatively, here is the result of using the statement `graph TB` in place of `graph LR`:

<img src="inst/img/mermaid_2.png">

Keep in mind that external files can also be called by the `mermaid` function. The file `graph.mmd` can contain the text of the diagram spec as follows

```
graph LR
  A-->B
  A-->C
  C-->E
  B-->D
  C-->D
  D-->F
  E-->F
```

and be rendered through:

```R
mermaid("graph.mmd")
```

Alright, here's another example. This one places some text inside the diagram objects. Also, there are some CSS styles to add a color fill to each of the diagram objects:

```R
diagram <- "
graph LR
A(Rounded)-->B[Squared]
B-->C{A Decision}
C-->D[Square One]
C-->E[Square Two]
    
style A fill:#DCEBE3
style B fill:#77DFC9
style C fill:#DEDBBA
style D fill:#F8F0CC
style E fill:#FCFCF2
"
    
mermaid(diagram)
```

What you get is this:

<img src="inst/img/mermaid_3.png">

Here's an example with line text (that is, text appearing on connecting lines). Simply place text between pipe characters, just after the arrow, right before the node identifier. There are few more CSS properties for the boxes included in this example (`stroke`, `stroke-width`, and `stroke-dasharray`).

```R
diagram <- "
graph BT
A(Start)-->|Line Text|B(Keep Going)
B-->|More Line Text|C(Stop)
    
style A fill:#A2EB86, stroke:#04C4AB, stroke-width:2px
style B fill:#FFF289, stroke:#FCFCFF, stroke-width:2px, stroke-dasharray: 4, 4
style C fill:#FFA070, stroke:#FF5E5E, stroke-width:2px
"

mermaid(diagram)
```

The resultant graphic:

<img src="inst/img/mermaid_4.png">

Let's include the values of some **R** objects into a fresh diagram. The `mtcars` dataset is something I go to again and again, so, I'm going to load it up.

```R
data(mtcars)
```

When you call the **R** `summary` function on this data frame, you obtain this:

```
     mpg             cyl             disp             hp             drat      
 Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0   Min.   :2.760  
 1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5   1st Qu.:3.080  
 Median :19.20   Median :6.000   Median :196.3   Median :123.0   Median :3.695  
 Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7   Mean   :3.597  
 3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0   3rd Qu.:3.920  
 Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0   Max.   :4.930  
       wt             qsec             vs               am              gear      
 Min.   :1.513   Min.   :14.50   Min.   :0.0000   Min.   :0.0000   Min.   :3.000  
 1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:3.000  
 Median :3.325   Median :17.71   Median :0.0000   Median :0.0000   Median :4.000  
 Mean   :3.217   Mean   :17.85   Mean   :0.4375   Mean   :0.4062   Mean   :3.688  
 3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:4.000  
 Max.   :5.424   Max.   :22.90   Max.   :1.0000   Max.   :1.0000   Max.   :5.000  
      carb      
 Min.   :1.000  
 1st Qu.:2.000  
 Median :2.000  
 Mean   :2.812  
 3rd Qu.:4.000  
 Max.   :8.000 
```

That information can placed into a diagram. First, we'll get a vector object for strings that specify each of the connections and the text inside the boxes (one for each `mtcars` dataset column). These strings will contain each of the statistics provided by the `summary` function (minimum, 1st quartile, median, mean, 3rd quartile, and maximum). We'll use a `sapply` to loop through each column.

```R
connections <- sapply(
  1:ncol(mtcars)
  , function(i){
    paste0(
      i
      , "(", colnames(mtcars)[i], ")---"
      , i, "-stats("
      , paste0(
        names(summary(mtcars[,i]))
        , ": "
        , unname(summary(mtcars[,i]))
        , collapse="<br/>"
      )
      , ")"
    )
  }
)
``` 

This generates all of the syntax required for connections between column names to the statistical summary text in each of the adjoining boxes. Notice the use of the `<br/>` tag that terminates each of the stats inside the `paste0` statement. They provide the necessary linebreaks for text within each diagram object.

Now, to generate the code for the summary diagram, one can use a `paste0` statement and then a separate `paste` statement for the connection text (with the `collapse` argument set to `\n` to specify a linebreak for the output text). Note that within the `paste0` statement, there is a `\n` linebreak wherever you would need one. Finally, to style multiple objects, a `classDef` statement was used. Here, a class of type `column` was provided with values for certain CSS properties. On the final line, the `class` statement applied the class definition to nodes 1 through 11 (a comma-separated list generated by the `paste0` statement). 

```R
diagram <-
paste0(
"graph TD;", "\n",
paste(connections, collapse = "\n"), "\n",
"classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
"class ", paste0(1:length(connections), collapse = ","), " column;
")

mermaid(diagram)
```

This is the resulting graphic:

<img src="inst/img/mermaid_5.png">

[Sequence diagrams](http://knsv.github.io/mermaid/sequenceDiagram.html) can be generated. The ["How to Draw Sequence Diagrams"](http://www.cs.uku.fi/research/publications/reports/A-2003-1/page91.pdf) report by Poranen, Makinen, and Nummenmaa offers a good introduction to sequence diagrams. Here's an example:

```R
# Using this "How to Draw a Sequence Diagram" 
# http://www.cs.uku.fi/research/publications/reports/A-2003-1/page91.pdf
# draw some sequence diagrams with DiagrammeR

mermaid("
sequenceDiagram
  Customer->>Ticket Seller: Ask for a Ticket
  Ticket Seller->>Database: Seats
  alt Tickets Are Available
    Database->>Ticket Seller: OK
    Ticket Seller->>Customer: Confirm
    Customer->>Ticket Seller: OK
    Ticket Seller->>Database: Book a Seat
    Ticket Seller->>Printer: Print a Ticket
  else Sold Out
    Database->>Ticket Seller: None Left
    Ticket Seller->>Customer: Sorry!
  end
")
```

<img src="inst/img/mermaid_6.png">

Gannt diagrams can also be generated. Here is an example of how to generate that type of project management diagram.

```R
mermaid("
gantt
dateFormat  YYYY-MM-DD
title A Very Nice Gantt Diagram

section Basic Tasks
This is completed                   :done,          first_1,    2014-01-06, 2014-01-08
This is active                      :active,        first_2,    2014-01-09, 3d
Do this later                       :               first_3,    after first_2, 5d
Do this after that                  :               first_4,    after first_3, 5d

section Important Things
Completed, critical task            :crit, done,    import_1,   2014-01-06,24h
Also done, also critical            :crit, done,    import_2,   after import_1, 2d
Doing this important task now       :crit, active,  import_3,   after import_2, 3d
Next critical task                  :crit,          import_4,   after import_3, 5d


section The Extras
First extras                        :active,        extras_1,   after import_4,  3d
Second helping                      :               extras_2,   after extras_1, 20h
More of the extras                  :               extras_3,   after extras_1, 48h

section The Wrap Up
Congratulations                     :               wrap_1,     after extras_3, 3d
Some meetings                       :                           5d
Additional meetings with cake       :                           18h
")
```

<img src="inst/img/mermaid_7.png">

### DiagrammeR + shiny

As with other **htmlwidgets**, we can easily dynamically bind **DiagrammeR** in **R** with **shiny**. Both `grViz` and `mermaid` (see table below) work with Shiny.

Using `grViz` with [`shinyAce`](https://github.com/trestletech/shinyAce), we can easily get an interactive playground for our graphviz diagram.


```R
library(shiny)
library(shinyAce)

ui <- shinyUI(fluidPage(fluidRow(
  column(
    width = 4
    , aceEditor("ace", value = "graph {}")
  ),
  column(
    width = 6
    , grVizOutput('diagram')
  )
)))

server <- function(input, output){
  output$diagram <- renderGrViz({
    grViz(
      input$ace
    )
  })
}

shinyApp(ui = ui, server = server)
```

<img src="inst/img/shiny_1.gif">

Here is a quick example where we can provide a `mermaid` diagram spec in a `textInput`.

```R
library(shiny)

ui = shinyUI(fluidPage(
  textInput('spec', 'Diagram Spec', value = ""),
  DiagrammeROutput('diagram')
))

server = function(input, output){
  output$diagram <- renderDiagrammeR(DiagrammeR(
    input$spec
  ))
}

shinyApp(ui = ui, server = server)
```

Not all browsers are currently compatible with the DiagrammeR `mermaid` Shiny app. The following table provides the status for a selection of current browsers.

|Browser/Version             | Platform                       | Status           |
|:---------------------------|:-------------------------------|:-----------------|
|IE 8                        | Windows                        | not working      |
|IE 9                        | Windows                        | not working      |
|IE 10                       | Windows                        | not working      |
|IE 11                       | Windows                        | not working      |
|Safari                      | Windows                        | not working      |
|Safari                      | Mac                            | not working      |
|RStudio Viewer              | Windows                        | not working      |
|RStudio Viewer              | Mac                            | not working      |
|Firefox                     | Windows                        | working          |
|Firefox                     | Mac                            | working          |
|Chrome                      | Windows                        | working          |
|Chrome                      | Mac                            | working          |

