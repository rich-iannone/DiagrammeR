# Create a graph object

Generates a graph object with the option to use node data frames (ndfs)
and/or edge data frames (edfs) to populate the initial graph.

## Usage

``` r
create_graph(
  nodes_df = NULL,
  edges_df = NULL,
  directed = TRUE,
  graph_name = NULL,
  attr_theme = "default",
  write_backups = FALSE,
  display_msgs = FALSE
)
```

## Arguments

- nodes_df:

  An optional data frame containing, at minimum, a column (called `id`)
  which contains node IDs for the graph. Additional columns (node
  attributes) can be included with values for the named node attribute.

- edges_df:

  An optional data frame containing, at minimum, two columns (called
  `from` and `to`) where node IDs are provided. Additional columns (edge
  attributes) can be included with values for the named edge attribute.

- directed:

  With `TRUE` (the default) or `FALSE`, either directed or undirected
  edge operations will be generated, respectively.

- graph_name:

  An optional string for labeling the graph object.

- attr_theme:

  The theme (i.e., collection of `graph`, `node`, and `edge` global
  graph attributes) to use for this graph. The default theme is called
  `default`; there are hierarchical layout themes called `lr`, `tb`,
  `rl`, and `bt` (these operate from left-to-right, top-to-bottom,
  right-to-left, and bottom-to-top); and, for larger graphs, the `fdp`
  theme provides a force directed layout. If this is set to `NULL` then
  no global graph attributes will be applied to the graph upon creation.

- write_backups:

  An option to write incremental backups of changing graph states to
  disk. If `TRUE`, a subdirectory within the working directory will be
  created and used to store `RDS` files. The default value is `FALSE` so
  one has to opt in to use this functionality.

- display_msgs:

  An option to display messages primarily concerned with changes in
  graph selections. By default, this is `FALSE`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# With `create_graph()` we can
# simply create an empty graph (and
# add in nodes and edges later
# with other functions)
graph <- create_graph()

# A graph can be created with
# nodes and without having any edges;
# this can be done in 2 steps:
# 1. create a node data frame (ndf)
#    using `create_node_df()`
ndf <-
  create_node_df(n = 4)

# 2. create a new graph object with
#    `create_graph()` and then pass
#    in the ndf to `nodes_df`
graph <-
  create_graph(
    nodes_df = ndf)

# Get information on the graph's nodes
graph |>
  get_node_info()
#>   id type label deg indeg outdeg loops
#> 1  1 <NA>  <NA>   0     0      0     0
#> 2  2 <NA>  <NA>   0     0      0     0
#> 3  3 <NA>  <NA>   0     0      0     0
#> 4  4 <NA>  <NA>   0     0      0     0

# You can create a similar graph with
# just nodes but also providing a
# range of attributes for the nodes
# (e.g., types, labels, or arbitrary
# 'values')
ndf <-
  create_node_df(
    n = 4,
    label = TRUE,
    type = c("type_1", "type_1",
             "type_5", "type_2"),
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    values = c(3.5, 2.6, 9.4, 2.7))

graph <-
  create_graph(nodes_df = ndf)

# Get information on the graph's
# internal node data frame (ndf)
graph |>
  get_node_df()
#>   id   type label     shape values
#> 1  1 type_1     1    circle    3.5
#> 2  2 type_1     2    circle    2.6
#> 3  3 type_5     3 rectangle    9.4
#> 4  4 type_2     4 rectangle    2.7

# A graph can also be created by
# specifying both the nodes and
# edges; create an edge data frame
# (edf) using the `create_edge_df()`
# function:
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to",
    values = c(7.3, 2.6, 8.3))

# Create the graph object with
# `create_graph()` and pass in the
# ndf and edf objects
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Get information on the graph's
# internal edge data frame (edf)
graph |> get_edge_df()
#>   id from to        rel values
#> 1  1    1  4 leading_to    7.3
#> 2  2    2  3 leading_to    2.6
#> 3  3    3  1 leading_to    8.3

# Get information on the graph's
# internal node data frame (ndf)
graph |> get_node_df()
#>   id   type label     shape values
#> 1  1 type_1     1    circle    3.5
#> 2  2 type_1     2    circle    2.6
#> 3  3 type_5     3 rectangle    9.4
#> 4  4 type_2     4 rectangle    2.7
```
