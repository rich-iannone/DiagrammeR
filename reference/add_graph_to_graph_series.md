# Add graph object to a graph series object

Add a graph object to an extant graph series object for storage of
multiple graphs across a sequential or temporal one-dimensional array.

## Usage

``` r
add_graph_to_graph_series(graph_series, graph)
```

## Arguments

- graph_series:

  A graph series object to which the graph object will be added.

- graph:

  A graph object to add to the graph series object.

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

# Count the number of graphs
# in the graph series
series |>
  count_graphs_in_graph_series()
#> [1] 3
```
