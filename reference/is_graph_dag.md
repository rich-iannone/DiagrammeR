# Is the graph a directed acyclic graph?

Provides a logical value on whether the graph is a directed acyclic
graph (DAG). The conditions for a graph that is a DAG are that it should
be a directed graph and it should not contain any cycles.

## Usage

``` r
is_graph_dag(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create a directed graph containing
# only a balanced tree
graph_tree <-
  create_graph() |>
  add_balanced_tree(
    k = 2, h = 3)

# Determine whether this graph
# is a DAG
graph_tree |>
  is_graph_dag()
#> [1] TRUE

# Create a directed graph containing
# a single cycle
graph_cycle <-
  create_graph() |>
  add_cycle(n = 5)

# Determine whether this graph
# is a DAG
graph_cycle |>
  is_graph_dag()
#> [1] FALSE

# Create an undirected graph
# containing a balanced tree
graph_tree_undirected <-
  create_graph(
    directed = FALSE) |>
  add_balanced_tree(
    k = 2, h = 2)

# Determine whether this graph
# is a DAG
graph_tree_undirected |>
  is_graph_dag()
#> [1] FALSE
```
