# Get a vector of edge ID values

Obtain a vector of edge ID values from a graph object. An optional
filter by edge attribute can limit the set of edge ID values returned.

## Usage

``` r
get_edge_ids(graph, conditions = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  an option to use filtering conditions for the retrieval of edges.

## Value

A vector of edge ID values.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "letter",
    color = c("red", "green", "grey", "blue"),
    value = c(3.5, 2.6, 9.4, 2.7))

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to",
    color = c("pink", "blue", "blue"),
    value = c(3.9, 2.5, 7.3))

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Get a vector of all edges in a graph
graph |> get_edge_ids()
#> [1] 1 2 3

# Get a vector of edge ID values using a
# numeric comparison (i.e., all edges with
# `value` attribute greater than 3)
get_edge_ids(
  graph,
  conditions = value > 3)
#> [1] 1 3

# Get a vector of edge ID values using
# a match pattern (i.e., all edges with
# `color` attribute of `pink`)
get_edge_ids(
  graph,
  conditions = color == "pink")
#> [1] 1

# Use multiple conditions to return edges
# with the desired attribute values
get_edge_ids(
  graph,
  conditions =
    color == "blue" &
    value > 5)
#> [1] 3
```
