# Apply a layout position to a single node

Apply position information for a single node. This is done by setting
the `x` and `y` attrs for a node `id` or node `label` supplied in
`node`. When rendering the graph, nodes with attribute values set for
`x` and `y` will be fixed to those positions on the graph canvas.

## Usage

``` r
set_node_position(graph, node, x, y, use_labels = FALSE)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  A single-length vector containing either a node ID value (integer) or
  a node label (character) for which position information should be
  applied.

- x:

  The x coordinate to set for the node.

- y:

  The y coordinate to set for the node.

- use_labels:

  An option to use a node `label` value in `node`. Note that this is
  only possible if all nodes have distinct `label` values set and none
  exist as an NA value.

## Value

A graph object of class `dgr_graph`.

## See also

Other node creation and removal:
[`add_n_node_clones()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_node_clones.md),
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
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md)

## Examples

``` r
# Create a simple graph with 4 nodes
graph <-
  create_graph() |>
  add_node(label = "one") |>
  add_node(label = "two") |>
  add_node(label = "three") |>
  add_node(label = "four")

# Add position information to each of
# the graph's nodes
graph <-
  graph |>
  set_node_position(
    node = 1,
    x = 1, y = 1) |>
  set_node_position(
    node = 2,
    x = 2, y = 2) |>
  set_node_position(
    node = 3,
    x = 3, y = 3) |>
  set_node_position(
    node = 4,
    x = 4, y = 4)

# View the graph's node data frame to
# verify that the `x` and `y` node
# attributes are available and set to
# the values provided
graph |> get_node_df()
#>   id type label x y
#> 1  1 <NA>   one 1 1
#> 2  2 <NA>   two 2 2
#> 3  3 <NA> three 3 3
#> 4  4 <NA>  four 4 4

# The same function can modify the data
# in the `x` and `y` attributes
graph <-
  graph |>
  set_node_position(
    node = 1,
    x = 1, y = 4) |>
  set_node_position(
    node = 2,
    x = 3, y = 3) |>
  set_node_position(
    node = 3,
    x = 3, y = 2) |>
  set_node_position(
    node = 4,
    x = 4, y = 1)

# View the graph's node data frame
graph |> get_node_df()
#>   id type label x y
#> 1  1 <NA>   one 1 4
#> 2  2 <NA>   two 3 3
#> 3  3 <NA> three 3 2
#> 4  4 <NA>  four 4 1

# Position changes can also be made by
# supplying a node `label` value (and setting
# `use_labels` to TRUE). For this to work,
# all `label` values in the graph's ndf must
# be unique and non-NA
graph <-
  graph |>
  set_node_position(
    node = "one",
    x = 1, y = 1,
    use_labels = TRUE) |>
  set_node_position(
    node = "two",
    x = 2, y = 2,
    use_labels = TRUE)

# View the graph's node data frame
graph |> get_node_df()
#>   id type label x y
#> 1  1 <NA>   one 1 1
#> 2  2 <NA>   two 2 2
#> 3  3 <NA> three 3 2
#> 4  4 <NA>  four 4 1
```
