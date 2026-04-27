# Get information on a graph series

Obtain a data frame with information on the graphs within a graph
series.

## Usage

``` r
get_graph_series_info(graph_series)
```

## Arguments

- graph_series:

  A graph series object of type `dgr_graph_1D`.

## Value

A data frame containing information on the graphs within the supplied
graph series.

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

# Get information on the graphs in the series
series |> get_graph_series_info()
#>   graph           name           date_time  tz nodes edges directed
#> 1     1 graph_SVUyR63K 2026-04-27 20:39:41 UTC     4     3     TRUE
#> 2     2 graph_SueySz3B 2026-04-27 20:39:41 UTC     5     5     TRUE
#> 3     3 graph_HYG7a9jw 2026-04-27 20:39:41 UTC     6     5     TRUE
```
