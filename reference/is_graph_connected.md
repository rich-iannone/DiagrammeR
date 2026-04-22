# Is the graph a connected graph?

Determines whether a graph is a connected graph.

## Usage

``` r
is_graph_connected(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function; this
# graph is not connected
create_graph() |>
  add_gnm_graph(
    n = 15,
    m = 10,
    set_seed = 23) |>
  is_graph_connected()
#> [1] FALSE

# Create another random graph;
# this graph is connected
create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23) |>
  is_graph_connected()
#> [1] TRUE
```
