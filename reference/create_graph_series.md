# Create a graph series object

Create a graph series object for the storage of multiple graphs across a
sequential or temporal one-dimensional array.

## Usage

``` r
create_graph_series(
  graph = NULL,
  series_name = NULL,
  series_type = "sequential"
)
```

## Arguments

- graph:

  A graph object to add to the new graph series object.

- series_name:

  An optional name to ascribe to the series.

- series_type:

  Either a `sequential` type (the default) or a `temporal` type (which
  requires date-time strings and time zone codes to be supplied).

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
