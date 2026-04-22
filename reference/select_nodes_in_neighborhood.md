# Select nodes based on a walk distance from a specified node

Select those nodes in the neighborhood of nodes connected a specified
distance from an initial node.

## Usage

``` r
select_nodes_in_neighborhood(graph, node, distance, set_op = "union")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  The node from which the traversal will originate.

- distance:

  The maximum number of steps from the `node` for inclusion in the
  selection.

- set_op:

  The set operation to perform upon consecutive selections of graph
  nodes. This can either be as a `union` (the default), as an
  intersection of selections with `intersect`, or, as a `difference` on
  the previous selection, if it exists.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph containing
# a balanced tree
graph <-
  create_graph() |>
  add_balanced_tree(
    k = 2, h = 2)

# Create a graph selection by
# selecting nodes in the
# neighborhood of node `1`, where
# the neighborhood is limited by
# nodes that are 1 connection
# away from node `1`
graph <-
  graph |>
  select_nodes_in_neighborhood(
    node = 1,
    distance = 1)

# Get the selection of nodes
graph |> get_selection()
#> [1] 1 2 3

# Perform another selection
# of nodes, this time with a
# neighborhood spanning 2 nodes
# from node `1`
graph <-
  graph |>
  clear_selection() |>
  select_nodes_in_neighborhood(
    node = 1,
    distance = 2)

# Get the selection of nodes
graph |> get_selection()
#> [1] 1 2 3 4 5 6 7
```
