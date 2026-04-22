# Get count of all loop edges

From a graph object of class `dgr_graph`, get a count of all loop edges
in the graph.

## Usage

``` r
count_loop_edges(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A numeric vector of single length.

## Examples

``` r
# Create an undirected, full graph
# with 3 nodes and all possible
# edges, including loop edges
graph <-
  create_graph(
    directed = FALSE) |>
  add_full_graph(
    n = 3,
    keep_loops = TRUE)

# Get a count of all loop edges
# in the graph
graph |> count_loop_edges()
#> [1] 3
```
