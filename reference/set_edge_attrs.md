# Set edge attribute values

From a graph object of class `dgr_graph`, set edge attribute values for
one or more edges.

## Usage

``` r
set_edge_attrs(graph, edge_attr, values, from = NULL, to = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr:

  The name of the attribute to set. Some examples are located in
  [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)

- values:

  The values to be set for the chosen attribute for the chosen edges.

- from:

  An optional vector of node IDs from which the edge is outgoing for
  filtering list of nodes with outgoing edges in the graph.

- to:

  An optional vector of node IDs from which the edge is incoming for
  filtering list of nodes with incoming edges in the graph.

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
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a simple graph
ndf <-
  create_node_df(
    n = 4,
    type = "basic",
    label = TRUE,
    value = c(3.5, 2.6, 9.4, 2.7))

edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to")

graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Set attribute `color = "green"`
# for edges `1`->`4` and `3`->`1`
# in the graph
graph <-
  graph |>
  set_edge_attrs(
    edge_attr = color,
    values = "green",
    from = c(1, 3),
    to = c(4, 1))

# Set attribute `color = "blue"`
# for all edges in the graph
graph <-
  graph |>
  set_edge_attrs(
    edge_attr = color,
    values = "blue")

# Set attribute `color = "pink"`
# for all edges in graph outbound
# from node with ID value `1`
graph <-
  graph |>
  set_edge_attrs(
    edge_attr = color,
    values = "pink",
    from = 1)

# Set attribute `color = "black"`
# for all edges in graph inbound
# to node with ID `1`
graph <-
  graph |>
  set_edge_attrs(
    edge_attr = color,
    values = "black",
    to = 1)
```
