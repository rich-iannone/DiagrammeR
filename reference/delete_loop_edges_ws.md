# Delete all loop edges associated with a selection of nodes

With a selection of nodes in a graph, remove any associated loop edges.

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
delete_loop_edges_ws(graph)
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
[`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md),
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
# Create an undirected, full graph
# of 5 nodes with loops retained
graph <-
  create_graph(
    directed = FALSE) |>
  add_full_graph(
    n = 5,
    keep_loops = TRUE)

# Select nodes `3` and `4`
# and remove the loop edges
# associated with those nodes
graph <-
  graph |>
  select_nodes_by_id(
    nodes = 3:4) |>
  delete_loop_edges_ws()

# Count the number of loop
# edges remaining in the graph
graph |> count_loop_edges()
#> [1] 3
```
