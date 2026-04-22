# Is the graph an undirected graph?

Determines whether a graph is set as undirected or not and returns a
logical value to that effect.

## Usage

``` r
is_graph_undirected(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create an empty graph; by
# default, new graphs made
# by `create_graph()` are
# directed graph, so, use
# `directed = FALSE` to create
# an undirected graph
graph <-
  create_graph(
    directed = FALSE)

# Determine whether the
# graph is undirected
graph |> is_graph_undirected()
#> [1] TRUE

# Use the `set_graph_directed()`
# function and check again
# as to whether the graph is
# undirected
graph |>
  set_graph_directed() |>
  is_graph_undirected()
#> [1] FALSE
```
