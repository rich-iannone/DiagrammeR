# Count graphs in a graph series object

Counts the total number of graphs in a graph series object.

## Usage

``` r
count_graphs_in_graph_series(graph_series)
```

## Arguments

- graph_series:

  A graph series object of type `dgr_graph_1D`

## Value

A numeric vector representing a count of graphs in a graph series
object.

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

# Count the number of graphs
# in the graph series
series |>
  count_graphs_in_graph_series()
#> [1] 3
```
