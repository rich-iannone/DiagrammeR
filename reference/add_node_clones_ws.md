# Add clones of a selection of nodes

Add new nodes to a graph object of class `dgr_graph` which are clones of
nodes in an active selection of nodes. All node attributes are preserved
except for the node `label` attribute (to maintain the uniqueness of
non-`NA` node label values). A vector of node `label` can be provided to
bind new labels to the cloned nodes.

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
add_node_clones_ws(graph, add_edges = FALSE, direction = NULL, label = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- add_edges:

  An option for whether to add edges from the selected nodes to each of
  their clones, or, in the opposite direction.

- direction:

  Using `from` will create new edges from existing nodes to the new,
  cloned nodes. The `to` option will create new edges directed toward
  the existing nodes.

- label:

  An optional vector of node label values. The vector length should
  correspond to the number of nodes in the active selection of nodes.

## Value

A graph object of class `dgr_graph`.

## See also

Other node creation and removal:
[`add_n_node_clones()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_node_clones.md),
[`add_n_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes.md),
[`add_n_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes_ws.md),
[`add_node()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node.md),
[`add_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node_df.md),
[`add_nodes_from_df_cols()`](https://rich-iannone.github.io/DiagrammeR/reference/add_nodes_from_df_cols.md),
[`add_nodes_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_nodes_from_table.md),
[`colorize_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_node_attrs.md),
[`copy_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_node_attrs.md),
[`create_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_node_df.md),
[`delete_node()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_node.md),
[`delete_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_nodes_ws.md),
[`drop_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_node_attrs.md),
[`join_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_node_attrs.md),
[`layout_nodes_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/layout_nodes_w_string.md),
[`mutate_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs.md),
[`mutate_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs_ws.md),
[`node_data()`](https://rich-iannone.github.io/DiagrammeR/reference/node_data.md),
[`recode_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_node_attrs.md),
[`rename_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_node_attrs.md),
[`rescale_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_node_attrs.md),
[`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md),
[`set_node_attr_w_fcn()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_w_fcn.md),
[`set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.md),
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md),
[`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md)

## Examples

``` r
# Create a graph with a path of
# nodes; supply `label`, `type`,
# and `value` node attributes,
# and select the created nodes
graph <-
  create_graph() |>
  add_path(
    n = 3,
    label = c("d", "g", "r"),
    type = c("a", "b", "c")) |>
  select_last_nodes_created()

# Display the graph's internal
# node data frame
graph |> get_node_df()
#>   id type label
#> 1  1    a     d
#> 2  2    b     g
#> 3  3    c     r

# Create clones of all nodes
# in the selection but assign
# new node label values
# (leaving `label` as NULL
# yields NA values)
graph <-
  graph |>
  add_node_clones_ws(
    label = c("a", "b", "v"))

# Display the graph's internal
# node data frame: nodes `4`,
# `5`, and `6` are clones of
# `1`, `2`, and `3`
graph |> get_node_df()
#>   id type label
#> 1  1    a     d
#> 2  2    b     g
#> 3  3    c     r
#> 4  4    a     a
#> 5  5    b     b
#> 6  6    c     v

# Select the last nodes
# created (`4`, `5`, and `6`)
# and clone those nodes and
# their attributes while
# creating new edges between
# the new and existing nodes
graph <-
  graph |>
  select_last_nodes_created() |>
  add_node_clones_ws(
    add_edges = TRUE,
    direction = "to",
    label = c("t", "z", "s"))

# Display the graph's internal
# edge data frame; there are
# edges between the selected
# nodes and their clones
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    4  7 <NA>
#> 4  4    5  8 <NA>
#> 5  5    6  9 <NA>
```
