# Determine whether a specified edge is present

From a graph object of class `dgr_graph`, determine whether an edge
(defined by a pair of node IDs or node label values) is present.

## Usage

``` r
is_edge_present(graph, edge = NULL, from = NULL, to = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge:

  An edge ID value to test for presence in the graph. If a single,
  numeric value is provided then values for `from` or `to` needn't be
  supplied.

- from:

  A node ID from which the edge is outgoing, or, the label associated
  with the node. For an undirected graph, the value in `from` can be
  interchangeable with that in `to`.

- to:

  A node ID to which the edge is incoming, or, the label associated with
  the node. For an undirected graph, the value in `to` can be
  interchangeable with that in `from`.

## Value

A logical value.

## Examples

``` r
# Create a simple graph with
# a path of four nodes
graph <-
  create_graph() |>
  add_path(
    n = 4,
    type = "path",
    label = c("one", "two",
              "three", "four"))

# Find out if edge ID `3`
# is present in the graph
graph |>
  is_edge_present(edge = 3)
#> [1] TRUE

# Determine if there are any edges
# with the definition `1` -> `2`
graph |>
  is_edge_present(
    from = 1,
    to = 2)
#> [1] TRUE

# Determine if there are any edges
# with the definition `4` -> `5`
graph |>
  is_edge_present(
    from = 4,
    to = 5)
#> [1] FALSE

# Determine whether an edge,
# defined by its labels as
# `two` -> `three`, exists in
# the graph
graph |>
  is_edge_present(
    from = "two",
    to = "three")
#> [1] TRUE

# Set the graph as undirected
# and determine whether an
# edge between nodes with labels
# `three` and `two` exists
graph |>
  set_graph_undirected() |>
  is_edge_present(
    from = "three",
    to = "two")
#> [1] TRUE
```
