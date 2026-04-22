# Get total degree distribution data for a graph

Get degree distribution data for a graph. Graph degree is represented as
a frequency of total degree values over all nodes in the graph.

## Usage

``` r
get_degree_distribution(graph, mode = "total")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- mode:

  using `total` (the default), degree considered for each node will be
  the total degree. With `in` and `out` the degree used will be the
  in-degree and out-degree, respectively.

## Value

A data frame with degree frequencies.

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

# Get the total degree
# distribution for the graph
graph |>
  get_degree_distribution(
    mode = "total")
#>   degree total_degree_dist
#> 1      0               0.1
#> 2      1               0.0
#> 3      2               0.2
#> 4      3               0.4
#> 5      4               0.1
#> 6      5               0.2
```
