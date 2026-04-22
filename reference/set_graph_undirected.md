# Convert a directed graph to an undirected graph

Take a graph which is directed and convert it to an undirected graph.

## Usage

``` r
set_graph_undirected(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph with a
# directed tree
graph <-
  create_graph() |>
  add_balanced_tree(
    k = 2, h = 2)

# Convert this graph from
# directed to undirected
graph <-
  graph |>
  set_graph_undirected()

# Perform a check on whether
# graph is directed
graph |> is_graph_directed()
#> [1] FALSE
```
