# Get all neighbors of one or more nodes

With one or more nodes, get the set of all neighboring nodes.

## Usage

``` r
get_nbrs(graph, nodes)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  A vector of node ID values.

## Value

A vector of node ID values.

## Examples

``` r
# Create a simple, directed graph with 5
# nodes and 4 edges
graph <-
  create_graph() |>
  add_path(n = 5)

# Find all neighbor nodes for node `2`
graph |> get_nbrs(nodes = 2)
#> [1] 1 3

# Find all neighbor nodes for nodes `1`
# and `5`
graph |> get_nbrs(nodes = c(1, 5))
#> [1] 2 4

# Color node `3` with purple, get its
# neighbors and color those nodes green
graph <-
  graph |>
  select_nodes_by_id(nodes = 3) |>
  set_node_attrs_ws(
    node_attr = color,
    value = "purple") |>
  clear_selection() |>
  select_nodes_by_id(
    nodes = get_nbrs(
      graph = graph,
      nodes = 3)) |>
  set_node_attrs_ws(
    node_attr = color,
    value = "green")
```
