# Get detailed information on edges

Obtain a data frame with detailed information on edges and their
interrelationships within the graph.

## Usage

``` r
get_edge_info(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame containing information specific to each edge within the
graph.

## Examples

``` r
# Create a simple graph
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 5, m = 10,
    set_seed = 23)

# Get information on the
# graph's edges
graph |> get_edge_info()
#>    id from to  rel
#> 1   1    1  3 <NA>
#> 2   2    2  1 <NA>
#> 3   3    2  5 <NA>
#> 4   4    2  3 <NA>
#> 5   5    3  1 <NA>
#> 6   6    3  2 <NA>
#> 7   7    3  5 <NA>
#> 8   8    3  4 <NA>
#> 9   9    4  1 <NA>
#> 10 10    5  4 <NA>
```
