# Get histogram data for a graph's degree frequency

Get histogram data for a graph's degree frequency. The bin width is set
to 1 and zero-value degrees are omitted from the output.

## Usage

``` r
get_degree_histogram(graph, mode = "total")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- mode:

  using `total` (the default), degree considered for each node will be
  the total degree. With `in` and `out` the degree used will be the
  in-degree and out-degree, respectively.

## Value

A data frame with degree counts.

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

# Get degree histogram data for
# the graph (reporting total degree)
graph |>
  get_degree_histogram(
    mode = "total")
#>   degree total_degree_hist
#> 1      0                 1
#> 2      1                 0
#> 3      2                 2
#> 4      3                 4
#> 5      4                 1
#> 6      5                 2
```
