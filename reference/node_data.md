# Insert node data attributes during node creation

This helper function should be invoked to provide values for the
namesake `node_data` argument, which is present in any function where
nodes are created.

## Usage

``` r
node_data(...)
```

## Arguments

- ...:

  Node data attributes provided as one or more named vectors.

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
# Create a new graph and add
# a path with several node
# data attributes
graph <-
  create_graph() |>
  add_path(
    n = 3,
    type = "path",
    node_data = node_data(
      hour = 5,
      index = c(1, 3, 2)))

# View the graph's internal
# node data frame; the node
# data attributes have been
# inserted
graph |> get_node_df()
#>   id type label hour index
#> 1  1 path     1    5     1
#> 2  2 path     2    5     3
#> 3  3 path     3    5     2
```
