# Get node eccentricities

Get a data frame with node eccentricity values.

## Usage

``` r
get_eccentricity(graph, mode = "out")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- mode:

  the mode with which the shortest paths to or from the given vertices
  should be calculated for directed graphs. If `out` (the default) then
  the shortest paths from the node, if `in` then only shortest paths to
  each node are considered. If `all` is used, then the corresponding
  undirected graph will be used and edge directions will be ignored. For
  undirected graphs, this argument is ignored.

## Value

A data frame containing eccentricity values by node ID value.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get the eccentricity values for
# all nodes in the graph
graph |> get_eccentricity()
#>    id eccentricity
#> 1   1            2
#> 2   2            3
#> 3   3            3
#> 4   4            2
#> 5   5            3
#> 6   6            2
#> 7   7            3
#> 8   8            3
#> 9   9            2
#> 10 10            0
```
