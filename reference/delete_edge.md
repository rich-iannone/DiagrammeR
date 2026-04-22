# Delete an edge from an existing graph object

From a graph object of class `dgr_graph`, delete an existing edge by
specifying either: (1) a pair of node IDs corresponding to the edge
(keeping into consideration the direction of the edge in a directed
graph), or (2) an edge ID.

## Usage

``` r
delete_edge(graph, from = NULL, to = NULL, id = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- from:

  a node ID from which the edge to be removed is outgoing. If an edge ID
  is provided to `id`, then this argument is ignored. There is the
  option to use a node `label` value here (and this must correspondingly
  also be done for the `to` argument) for defining node connections.
  Note that this is only possible if all nodes have distinct `label`
  values set and none exist as an empty string.

- to:

  a node ID to which the edge to be removed is incoming. If an edge ID
  is provided to `id`, then this argument is ignored. There is the
  option to use a node `label` value here (and this must correspondingly
  also be for the `from` argument) for defining node connections. Note
  that this is only possible if all nodes have distinct `label` values
  set and none exist as an empty string.

- id:

  an edge ID of the edge to be removed.

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
[`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md),
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
# Create a graph with 2 nodes
graph <-
  create_graph() |>
  add_n_nodes(n = 2)

# Add an edge
graph <-
  graph |>
  add_edge(
    from = 1,
    to = 2)

# Delete the edge
graph <-
  graph |>
  delete_edge(
    from = 1,
    to = 2)

# Get the count of edges in the graph
graph |> count_edges()
#> [1] 0

# Create an undirected graph with
# 2 nodes and an edge
graph_undirected <-
  create_graph(directed = FALSE) |>
  add_n_nodes(n = 2) |>
  add_edge(
    from = 1,
    to = 2)

# Delete the edge; the order of node ID
# values provided in `from` and `to`
# don't matter for the undirected case
graph_undirected |>
  delete_edge(
    from = 2,
    to = 1) |>
  count_edges()
#> [1] 0

# The undirected graph has a single
# edge with ID `1`; it can be
# deleted by specifying `id`
graph_undirected |>
  delete_edge(id = 1) |>
  count_edges()
#> [1] 0

# Create a directed graph with 2
# labeled nodes and an edge
graph_labeled_nodes <-
  create_graph() |>
  add_n_nodes(
    n = 2,
    label = c("one", "two")) |>
  add_edge(
    from = "one",
    to = "two")

# Delete the edge using the node
# labels in `from` and `to`; this
# is analogous to creating the
# edge using node labels
graph_labeled_nodes |>
  delete_edge(
    from = "one",
    to = "two") |>
  count_edges()
#> [1] 0
```
