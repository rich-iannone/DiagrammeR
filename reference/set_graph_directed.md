# Convert an undirected graph to a directed graph

Take a graph which is undirected and convert it to a directed graph.

## Usage

``` r
set_graph_directed(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph with a
# undirected tree
graph <-
  create_graph(
    directed = FALSE) |>
  add_balanced_tree(
    k = 2, h = 2)

# Convert this graph from
# undirected to directed
graph <-
  graph |>
  set_graph_directed()

# Perform a check on whether
# graph is directed
graph |> is_graph_directed()
#> [1] TRUE
```
