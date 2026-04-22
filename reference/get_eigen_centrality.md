# Get the eigen centrality for all nodes

Get the eigen centrality values for all nodes in the graph.

## Usage

``` r
get_eigen_centrality(graph, weights_attr = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- weights_attr:

  An optional name of the edge attribute to use in the adjacency matrix.
  If `NULL` then, if it exists, the `weight` edge attribute of the graph
  will be used. If `NA` then no edge weights will be used.

## Value

A data frame with eigen centrality scores for each of the nodes.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 10, m = 15,
    set_seed = 23)

# Get the eigen centrality scores
# for nodes in the graph
graph |> get_eigen_centrality()
#>    id eigen_centrality
#> 1   1           0.6640
#> 2   2           0.6767
#> 3   3           0.4988
#> 4   4           0.9541
#> 5   5           0.7908
#> 6   6           1.0000
#> 7   7           0.6391
#> 8   8           0.4524
#> 9   9           0.6702
#> 10 10           0.0000
```
