# Get a count of all edges

From a graph object of class `dgr_graph`, get a count of edges in the
graph.

## Usage

``` r
count_edges(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single-length numeric vector.

## Examples

``` r
# Create a graph with a
# path of nodes and 3
# unconnected nodes
graph <-
  create_graph() |>
  add_path(n = 3) |>
  add_n_nodes(n = 3)

# Get a count of all edges
# in the graph
graph |>
  count_edges()
#> [1] 2
```
