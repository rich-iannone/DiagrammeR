# Get detailed information on nodes

Obtain a data frame with detailed information on nodes and their
interrelationships within the graph.

## Usage

``` r
get_node_info(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame containing information specific to each node within the
graph.

## Examples

``` r
# Create a simple graph
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 5, m = 10,
    set_seed = 23)

# Get information on the graph's nodes
graph |> get_node_info()
#>   id type label deg indeg outdeg loops
#> 1  1 <NA>     1   4     3      1     0
#> 2  2 <NA>     2   4     1      3     0
#> 3  3 <NA>     3   6     2      4     0
#> 4  4 <NA>     4   3     2      1     0
#> 5  5 <NA>     5   3     2      1     0
```
