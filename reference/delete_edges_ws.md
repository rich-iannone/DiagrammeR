# Delete all selected edges in an edge selection

In a graph object of class `dgr_graph`, delete all edges present in a
selection.

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
delete_edges_ws(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
[`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md),
[`add_edges_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_w_string.md),
[`add_forward_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_forward_edges_ws.md),
[`add_reverse_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_reverse_edges_ws.md),
[`copy_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_edge_attrs.md),
[`create_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_edge_df.md),
[`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md),
[`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md),
[`drop_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_edge_attrs.md),
[`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md),
[`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md),
[`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md),
[`mutate_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs_ws.md),
[`recode_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_edge_attrs.md),
[`rename_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_edge_attrs.md),
[`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md),
[`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md),
[`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md),
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a graph
graph <-
  create_graph() |>
  add_n_nodes(n = 3) |>
  add_edges_w_string(
    edges = "1->3 1->2 2->3")

# Select edges attached to
# node with ID `3` (these are
# `1`->`3` and `2`->`3`)
graph <-
  graph |>
  select_edges_by_node_id(nodes = 3)

# Delete edges in selection
graph <-
  graph |>
  delete_edges_ws()

# Get a count of edges in the graph
graph |> count_edges()
#> [1] 1
```
