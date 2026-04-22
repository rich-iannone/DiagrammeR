# Is the graph a directed graph?

Determines whether a graph is set to be directed or not and returns a
logical value to that effect.

## Usage

``` r
is_graph_directed(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create an empty graph; by default,
# new graphs made by `create_graph()`
# are directed
graph <- create_graph()

# Determine whether the graph
# is directed
graph |> is_graph_directed()
#> [1] TRUE

# Use the `set_graph_undirected()`
# function and check again whether
# the graph is directed
graph |>
  set_graph_undirected() |>
  is_graph_directed()
#> [1] FALSE
```
