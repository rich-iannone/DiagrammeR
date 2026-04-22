# Select edges in a graph using node ID values

Select edges in a graph object of class `dgr_graph` using node ID
values. All edges associated with the provided nodes will be included in
the selection.

## Usage

``` r
select_edges_by_node_id(graph, nodes, set_op = "union")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  A vector of node IDs for the selection of edges present in the graph.

- set_op:

  The set operation to perform upon consecutive selections of graph
  edges This can either be as a `union` (the default), as an
  intersection of selections with `intersect`, or, as a `difference` on
  the previous selection, if it exists.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph with 5 nodes
graph <-
  create_graph() |>
  add_path(n = 5)

# Create a graph selection by selecting edges
# associated with nodes `1` and `2`
graph <-
  graph |>
  select_edges_by_node_id(
    nodes = 1:2)

# Get the selection of edges
graph |> get_selection()
#> [1] 1 2

# Perform another selection of edges, with nodes
# `1`, `2`, and `4`
graph <-
  graph |>
  clear_selection() |>
  select_edges_by_node_id(
    nodes = c(1, 2, 4))

# Get the selection of edges
graph |> get_selection()
#> [1] 1 2 3 4

# Get a fraction of the edges selected over all
# the edges in the graph
l <- graph |>
  get_selection() |>
  length()

e <- graph |> count_edges()

l/e
#> [1] 1
```
