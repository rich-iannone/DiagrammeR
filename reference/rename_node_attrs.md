# Rename a node attribute

Within a graph's internal node data frame (ndf), rename an existing node
attribute.

## Usage

``` r
rename_node_attrs(graph, node_attr_from, node_attr_to)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr_from:

  The name of the node attribute that will be renamed.

- node_attr_to:

  The new name of the node attribute column identified in
  `node_attr_from`.

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
[`rescale_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_node_attrs.md),
[`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md),
[`set_node_attr_w_fcn()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_w_fcn.md),
[`set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.md),
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md),
[`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md)

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 5,
    m = 8,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = shape,
    values = "circle") |>
  set_node_attrs(
    node_attr = value,
    values = rnorm(
      n = 5,
      mean = 5,
      sd = 1) |> round(1))

# Get the graph's internal ndf
# to show which node attributes
# are available
graph |> get_node_df()
#>   id type label  shape value
#> 1  1 <NA>     1 circle   5.2
#> 2  2 <NA>     2 circle   5.3
#> 3  3 <NA>     3 circle   4.4
#> 4  4 <NA>     4 circle   5.8
#> 5  5 <NA>     5 circle   5.9

# Rename the `value` node
# attribute as `weight`
graph <-
  graph |>
  rename_node_attrs(
    node_attr_from = value,
    node_attr_to = weight)

# Get the graph's internal
# ndf to show that the node
# attribute had been renamed
graph |> get_node_df()
#>   id type label  shape weight
#> 1  1 <NA>     1 circle    5.2
#> 2  2 <NA>     2 circle    5.3
#> 3  3 <NA>     3 circle    4.4
#> 4  4 <NA>     4 circle    5.8
#> 5  5 <NA>     5 circle    5.9
```
