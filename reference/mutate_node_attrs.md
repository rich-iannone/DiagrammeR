# Mutate a set of node attribute values

Within a graph's internal node data frame (ndf), mutate numeric node
attribute values using one or more expressions.

## Usage

``` r
mutate_node_attrs(graph, ...)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- ...:

  Expressions used for the mutation of node attributes. LHS of each
  expression is either an existing or new node attribute name. The RHS
  can consist of any valid R code that uses node attributes as
  variables. Expressions are evaluated in the order provided, so, node
  attributes created or modified are ready to use in subsequent
  expressions.

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
# Create a graph with 3 nodes
graph <-
  create_graph() |>
  add_path(n = 3) |>
  set_node_attrs(
    node_attr = width,
    values = c(1.4, 0.3, 1.1))

# Get the graph's internal ndf
# to show which node attributes
# are available
graph |> get_node_df()
#>   id type label width
#> 1  1 <NA>     1   1.4
#> 2  2 <NA>     2   0.3
#> 3  3 <NA>     3   1.1

# Mutate the `width` node
# attribute, dividing each
# value by 2
graph <-
  graph |>
  mutate_node_attrs(
    width = width / 2)

# Get the graph's internal
# ndf to show that the node
# attribute `width` had its
# values changed
graph |> get_node_df()
#>   id type label width
#> 1  1 <NA>     1  0.70
#> 2  2 <NA>     2  0.15
#> 3  3 <NA>     3  0.55

# Create a new node attribute,
# called `length`, that is the
# log of values in `width` plus
# 2 (and, also, round all values
# to 2 decimal places)
graph <-
  graph |>
  mutate_node_attrs(
    length = (log(width) + 2) |>
               round(2))

# Get the graph's internal ndf
# to show that the node attribute
# values had been mutated
graph |> get_node_df()
#>   id type label width length
#> 1  1 <NA>     1  0.70   1.64
#> 2  2 <NA>     2  0.15   0.10
#> 3  3 <NA>     3  0.55   1.40

# Create a new node attribute
# called `area`, which is the
# product of the `width` and
# `length` attributes
graph <-
  graph |>
  mutate_node_attrs(
    area = width * length)

# Get the graph's internal ndf
# to show that the node attribute
# values had been multiplied
# together (with new attr `area`)
graph |> get_node_df()
#>   id type label width length  area
#> 1  1 <NA>     1  0.70   1.64 1.148
#> 2  2 <NA>     2  0.15   0.10 0.015
#> 3  3 <NA>     3  0.55   1.40 0.770
```
