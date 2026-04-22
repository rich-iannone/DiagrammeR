# Get a count of all nodes

From a graph object of class `dgr_graph`, get a count of nodes in the
graph.

## Usage

``` r
count_nodes(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A numeric vector of single length.

## Examples

``` r
# Create a graph with a
# path of nodes and 3
# unconnected nodes
graph <-
  create_graph() |>
  add_path(n = 3) |>
  add_n_nodes(n = 3)

# Get a count of all nodes
# in the graph
graph |>
  count_nodes()
#> [1] 6
```
