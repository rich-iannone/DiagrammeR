# Select nodes in a graph by their ID values

Select nodes in a graph object of class `dgr_graph` by their node ID
values. If nodes have IDs that are monotonically increasing integer
values, then numeric ranges can be used for the selection.

## Usage

``` r
select_nodes_by_id(graph, nodes, set_op = "union")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  A vector of node IDs for the selection of nodes present in the graph.

- set_op:

  The set operation to perform upon consecutive selections of graph
  nodes. This can either be as a `union` (the default), as an
  intersection of selections with `intersect`, or, as a `difference` on
  the previous selection, if it exists.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <- create_node_df(n = 10)

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf)

# Select nodes `1` to `5` and show that
# selection of nodes with `get_selection()`
graph |>
  select_nodes_by_id(nodes = 1:5) |>
  get_selection()
#> [1] 1 2 3 4 5
```
