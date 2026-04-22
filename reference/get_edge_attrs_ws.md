# Get edge attribute values from a selection of edges

From a graph object of class `dgr_graph`, get edge attribute values for
one or more edges.

This function makes use of an active selection of edges (and the
function ending with `_ws` hints at this).

Selections of edges can be performed using the following selection
(`select_*()`) functions:
[`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md),
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md),
[`select_edges_by_edge_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_edge_id.md),
or
[`select_edges_by_node_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_node_id.md).

Selections of edges can also be performed using the following traversal
(`trav_*()`) functions:
[`trav_out_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_edge.md),
[`trav_in_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_edge.md),
[`trav_both_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both_edge.md),
or
[`trav_reverse_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_reverse_edge.md).

## Usage

``` r
get_edge_attrs_ws(graph, edge_attr)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr:

  the name of the attribute for which to get values.

## Value

A named vector of edge attribute values for the attribute given by
`edge_attr` by edge.

## Examples

``` r
# Create a simple graph where
# edges have an edge attribute
# named `value`
graph <-
  create_graph() |>
  add_n_nodes(n = 4)

edges <-
  create_edge_df(
    from = c(1, 2, 1, 4),
      to = c(2, 3, 4, 3),
     rel = "rel")

graph <-
  add_edge_df(
    graph = graph,
    edge_df = edges) |>
  set_edge_attrs(
    edge_attr = value,
    values = 1.6,
    from = 1,
      to = 2) |>
  set_edge_attrs(
    edge_attr = value,
    values = 4.3,
    from = 1,
      to = 4) |>
  set_edge_attrs(
    edge_attr = value,
    values = 2.9,
    from = 2,
      to = 3) |>
  set_edge_attrs(
    edge_attr = value,
    values = 8.4,
    from = 4,
      to = 3)

# Select the edges defined as
# `1`->`3` and `2`->`3`
graph <-
  graph |>
  select_edges(
    from = c(1, 2),
    to = c(2, 3))

# Get the edge attribute values
# for the `value` attribute, limited
# to the current edge selection
graph |>
  get_edge_attrs_ws(
    edge_attr = value)
#> 1->2 2->3 
#>  1.6  2.9 
```
