# Remove a graph from a graph series

Remove a single graph object from an set of graph objects contained
within a graph series object.

## Usage

``` r
remove_graph_from_graph_series(graph_series, index = "last")
```

## Arguments

- graph_series:

  A graph series object from which the graph object will be removed.

- index:

  The index of the graph object to be removed from the graph series
  object.

## Value

A graph series object of type `dgr_graph_1D`.

## Examples

``` r
# Create three graphs
graph_1 <-
  create_graph() |>
  add_path(n = 4)

graph_2 <-
  create_graph() |>
  add_cycle(n = 5)

graph_3 <-
  create_graph() |>
  add_star(n = 6)

# Create an empty graph series
# and add the graphs
series <-
  create_graph_series() |>
  add_graph_to_graph_series(
    graph = graph_1) |>
  add_graph_to_graph_series(
    graph = graph_2) |>
  add_graph_to_graph_series(
    graph = graph_3)

# Remove the second graph
# from the graph series
series <-
  series |>
  remove_graph_from_graph_series(
    index = 2)

# With `get_graph_series_info()`,
# we can ensure that a graph
# was removed
series |>
  get_graph_series_info()
#>   graph           name           date_time  tz nodes edges directed
#> 1     1 graph_mkRJxXyq 2026-04-22 00:41:51 UTC     4     3     TRUE
#> 2     2 graph_ySz3BP7j 2026-04-22 00:41:51 UTC     6     5     TRUE
```
