# Add an edge between nodes in a graph object

With a graph object of class `dgr_graph`, add an edge to nodes within
the graph.

## Usage

``` r
add_edge(graph, from, to, rel = NULL, edge_aes = NULL, edge_data = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- from:

  The outgoing node from which the edge is connected. There is the
  option to use a node `label` value here (and this must correspondingly
  also be done for the `to` argument) for defining node connections.
  Note that this is only possible if all nodes have distinct `label`
  values set and none exist as an empty string.

- to:

  The incoming nodes to which each edge is connected. There is the
  option to use a node `label` value here (and this must correspondingly
  also be done for the `from` argument) for defining node connections.
  Note that this is only possible if all nodes have distinct `label`
  values set and none exist as an empty string.

- rel:

  An optional string specifying the relationship between the connected
  nodes.

- edge_aes:

  An optional list of named vectors comprising edge aesthetic
  attributes. The helper function
  [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)
  is strongly recommended for use here as it contains arguments for each
  of the accepted edge aesthetic attributes (e.g., `shape`, `style`,
  `penwidth`, `color`).

- edge_data:

  An optional list of named vectors comprising edge data attributes. The
  helper function
  [`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md)
  is strongly recommended for use here as it helps bind data
  specifically to the created edges.

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
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
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a graph with 4 nodes
graph <-
  create_graph() |>
  add_node(label = "one") |>
  add_node(label = "two") |>
  add_node(label = "three") |>
  add_node(label = "four")

# Add an edge between those
# nodes and attach a
# relationship to the edge
graph <-
 add_edge(
   graph,
   from = 1,
   to = 2,
   rel = "A")

# Use the `get_edge_info()`
# function to verify that
# the edge has been created
graph |>
  get_edge_info()
#>   id from to rel
#> 1  1    1  2   A

# Add another node and
# edge to the graph
graph <-
  graph |>
  add_edge(
    from = 3,
    to = 2,
    rel = "A")

# Verify that the edge
# has been created by
# counting graph edges
graph |> count_edges()
#> [1] 2

# Add edges by specifying
# node `label` values; note
# that all nodes must have
# unique `label` values to
# use this option
graph <-
  graph |>
  add_edge(
    from = "three",
    to = "four",
    rel = "L") |>
  add_edge(
    from = "four",
    to = "one",
    rel = "L")

# Use `get_edges()` to verify
# that the edges were added
graph |> get_edges()
#> [1] "1->2" "3->2" "3->4" "4->1"

# Add edge aesthetic and data
# attributes during edge creation
graph_2 <-
  create_graph() |>
  add_n_nodes(n = 2) |>
  add_edge(
    from = 1,
    to = 2,
    rel = "M",
    edge_aes = edge_aes(
      penwidth = 1.5,
      color = "blue"),
    edge_data = edge_data(
      value = 4.3))

# Use the `get_edges()` function
# to verify that the attribute
# values were bound to the
# newly created edge
graph_2 |> get_edge_df()
#>   id from to rel penwidth color value
#> 1  1    1  2   M      1.5  blue   4.3

```
