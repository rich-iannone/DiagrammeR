# Get node IDs for predecessor nodes to the specified node

Provides a vector of node IDs for all nodes that have a connection to
the given node.

## Usage

``` r
get_predecessors(graph, node)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  A node ID for the selected node.

## Value

A vector of node ID values.

## Examples

``` r
# Set a seed
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a node data frame (ndf)
ndf <- create_node_df(n = 26)

# Create an edge data
# frame (edf)
edf <-
  create_edge_df(
    from = sample(
      1:26, replace = TRUE),
    to = sample(
      1:26, replace = TRUE))

# From the ndf and edf,
# create a graph object
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Get predecessors for node
# `23` in the graph
graph |>
  get_predecessors(
    node = 23)
#> [1] 6

# If there are no predecessors,
# `NA` is returned
graph |>
  get_predecessors(
    node = 26)
#> [1] NA
```
