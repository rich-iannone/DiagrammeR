# Add one or more edges using a text string

With a graph object of class `dgr_graph`, add one or more edges to the
graph using a text string.

## Usage

``` r
add_edges_w_string(graph, edges, rel = NULL, use_labels = FALSE)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edges:

  A single-length vector with a character string specifying the edges.
  For a directed graph, the string object should be formatted as a
  series of node ID values as `[node_ID_1]->[node_ID_2]` separated by a
  one or more space characters. For undirected graphs, `--` should
  replace `->`. Line breaks in the vector won't cause an error.

- rel:

  An optional vector specifying the relationship between the connected
  nodes.

- use_labels:

  An option to use node `label` values in the `edges` string to define
  node connections. Note that this is only possible if all nodes have
  distinct `label` values set and none exist as an empty string.

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
[`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md),
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

# Add edges between nodes using
# a character string with node
# ID values
graph_node_id <-
  graph |>
  add_edges_w_string(
    edges = "1->2 1->3 2->4 2->3")

# Show the graph's internal
# edge data frame
graph_node_id |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    1  3 <NA>
#> 3  3    2  4 <NA>
#> 4  4    2  3 <NA>

# Add edges between nodes using
# a character string with node
# label values and setting
# `use_labels = TRUE`; note that
# all nodes must have unique
# `label` values to use this
graph_node_label <-
  graph |>
  add_edges_w_string(
    edges =
      "one->two one->three
       two->four two->three",
    use_labels = TRUE)

# Show the graph's internal
# edge data frame (it's the
# same as before)
graph_node_label |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    1  3 <NA>
#> 3  3    2  4 <NA>
#> 4  4    2  3 <NA>
```
