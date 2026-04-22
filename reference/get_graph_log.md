# Get the graph log information

Get a tibble of the graph log, which contains information on the
functions called on the graph that resulted in some transformation of
the graph.

## Usage

``` r
get_graph_log(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A `df_tbl` object.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function and
# delete 2 nodes from the graph
graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23) |>
  delete_node(node = 5) |>
  delete_node(node = 7)

# Get the graph log, which is a
# record of all graph transformations
graph |> get_graph_log()
#> # A tibble: 7 × 8
#>   version_id function_used  time_modified       duration nodes edges   d_n   d_e
#>        <int> <chr>          <dttm>                 <dbl> <int> <int> <int> <int>
#> 1          1 "create_graph" 2026-04-22 00:06:09  0.00274     0     0     0     0
#> 2          2 "add_gnm_grap… 2026-04-22 00:06:09  0.0106     10    15    10    15
#> 3          2 ""             2026-04-22 00:06:09  0.0106     10    15    10    15
#> 4          4 "delete_node"  2026-04-22 00:06:09  0.0139      9    11    -1    -4
#> 5          4 ""             2026-04-22 00:06:09  0.0139      9    11    -1    -4
#> 6          6 "delete_node"  2026-04-22 00:06:09  0.0168      8     8    -1    -3
#> 7          6 ""             2026-04-22 00:06:09  0.0168      8     8    -1    -3
```
