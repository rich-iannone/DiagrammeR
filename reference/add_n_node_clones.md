# Add one or several clones of an existing node to the graph

Add `n` new nodes to a graph object of class `dgr_graph` which are
clones of a node already in the graph. All node attributes are preserved
except for the node `label` attribute (to maintain the uniqueness of
non-`NA` node label values). A vector of node `label` can be provided to
bind new labels to the cloned nodes.

## Usage

``` r
add_n_node_clones(graph, n, node, label = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- n:

  The number of node clones to add to the graph.

- node:

  A node ID corresponding to the graph node to be cloned.

- label:

  An optional vector of node label values. The vector length should
  correspond to the value set for `n`.

## Value

A graph object of class `dgr_graph`.

## See also

Other node creation and removal:
[`add_n_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes.md),
[`add_n_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes_ws.md),
[`add_node()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node.md),
[`add_node_clones_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node_clones_ws.md),
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
# and `value` node attributes
graph <-
  create_graph() |>
  add_path(
    n = 3,
    label = c("d", "g", "r"),
    type = c("a", "b", "c"))

# Display the graph's internal
# node data frame
graph |> get_node_df()
#>   id type label
#> 1  1    a     d
#> 2  2    b     g
#> 3  3    c     r

# Create 3 clones of node `1`
# but assign new node label
# values (leaving `label` as
# NULL yields NA values)
graph <-
  graph |>
  add_n_node_clones(
    n = 3,
    node = 1,
    label = c("x", "y", "z"))

# Display the graph's internal
# node data frame: nodes `4`,
# `5`, and `6` are clones of `1`
graph |> get_node_df()
#>   id type label
#> 1  1    a     d
#> 2  2    b     g
#> 3  3    c     r
#> 4  4    a     x
#> 5  5    a     y
#> 6  6    a     z
```
