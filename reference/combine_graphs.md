# Combine two graphs into a single graph

Combine two graphs in order to make a new graph.

## Usage

``` r
combine_graphs(x, y)
```

## Arguments

- x:

  A `DiagrammeR` graph object to which another graph will be unioned.
  This graph should be considered the graph from which global graph
  attributes will be inherited in the resulting graph.

- y:

  A `DiagrammeR` graph object that is to be unioned with the graph
  supplied as `x`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph with a cycle
# containing 6 nodes
graph_cycle <-
 create_graph() |>
   add_cycle(n = 6)

# Create a random graph with
# 8 nodes and 15 edges using the
# `add_gnm_graph()` function
graph_random <-
  create_graph() |>
  add_gnm_graph(
    n = 8,
    m = 15,
    set_seed = 23)

# Combine the two graphs in a
# union operation
combined_graph <-
  combine_graphs(
    graph_cycle,
    graph_random)

# Get the number of nodes in
# the combined graph
combined_graph |> count_nodes()
#> [1] 14

# The `combine_graphs()`
# function will renumber
# node ID values in graph `y`
# during the union; this ensures
# that node ID values are unique
combined_graph |> get_node_ids()
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
```
