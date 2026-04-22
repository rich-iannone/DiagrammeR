# Create a node data frame

Combine several vectors for nodes and their attributes into a data
frame, which can be combined with other similarly-generated data frames,
or, added to a graph object. A node data frame, or ndf, has at least the
following columns:

- `id` (of type `integer`)

- `type` (of type `character`)

- `label` (of type `character`)

An arbitrary number of additional columns containing aesthetic or data
attributes can be part of the ndf, see
[`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md)
for additional attributes that can be used with ..., so long as they
follow the aforementioned columns.

## Usage

``` r
create_node_df(n, type = NULL, label = NULL, ...)
```

## Arguments

- n:

  The total number of nodes to include in the node data frame.

- type:

  An optional `type` for each node.

- label:

  An optional `label` for each node.

- ...:

  Additional attributes. Some are present in
  [`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md)

## Value

A node data frame (ndf).

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
# Create a node data frame (ndf) where the labels
# are equivalent to the node ID values (this is not
# recommended); the `label` and `type` node
# attributes will always be a `character` class
# whereas `id` will always be an `integer`
node_df <-
  create_node_df(
    n = 4,
    type = c("a", "a", "b", "b"),
    label = TRUE)

# Display the node data frame
node_df
#>   id type label
#> 1  1    a     1
#> 2  2    a     2
#> 3  3    b     3
#> 4  4    b     4

# Create an ndf with distinct labels and
# additional node attributes (where their classes
# will be inferred from the input vectors)
node_df <-
  create_node_df(
    n = 4,
    type = "a",
    label = c(2384, 3942, 8362, 2194),
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    value = c(3.5, 2.6, 9.4, 2.7))

# Display the node data frame
node_df
#>   id type label  style color     shape value
#> 1  1    a  2384 filled  aqua    circle   3.5
#> 2  2    a  3942 filled  aqua    circle   2.6
#> 3  3    a  8362 filled  aqua rectangle   9.4
#> 4  4    a  2194 filled  aqua rectangle   2.7
```
