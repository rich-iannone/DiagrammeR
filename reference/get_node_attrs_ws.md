# Get node attribute values from a selection of nodes

From a graph object of class `dgr_graph`, get node attribute values from
nodes currently active as a selection.

This function makes use of an active selection of nodes (and the
function ending with `_ws` hints at this).

Selections of nodes can be performed using the following node selection
(`select_*()`) functions:
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md),
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md),
[`select_nodes_by_degree()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_degree.md),
[`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md),
or
[`select_nodes_in_neighborhood()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_in_neighborhood.md).

Selections of nodes can also be performed using the following traversal
(`trav_*()`) functions:
[`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md),
[`trav_in()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in.md),
[`trav_both()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both.md),
[`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md),
[`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
[`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md),
or
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md).

## Usage

``` r
get_node_attrs_ws(graph, node_attr)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr:

  The name of the attribute for which to get values.

## Value

A named vector of node attribute values for the attribute given by
`node_attr` by node ID.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 4,
    m = 4,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = value,
    values = c(2.5, 8.2, 4.2, 2.4))

# Select nodes with ID values
# `1` and `3`
graph <-
  graph |>
  select_nodes_by_id(
    nodes = c(1, 3))

# Get the node attribute values
# for the `value` attribute, limited
# to the current node selection
graph |>
  get_node_attrs_ws(
    node_attr = value)
#>   1   3 
#> 2.5 4.2 
```
