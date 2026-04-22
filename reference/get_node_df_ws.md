# Get the graph's ndf filtered by a selection of nodes

From a graph object of class `dgr_graph`, get the graph's internal node
data frame that is filtered by the node ID values currently active as a
selection.

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
get_node_df_ws(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A node data frame.

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

# Get the node data frame that's
# limited to the rows that correspond
# to the node selection
graph |> get_node_df_ws()
#>   id type label value
#> 1  1 <NA>     1   2.5
#> 2  3 <NA>     3   4.2
```
