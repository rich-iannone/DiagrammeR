# Recode a set of node attribute values

Within a graph's internal node data frame (ndf), recode character or
numeric node attribute values. Optionally, one can specify a replacement
value for any unmatched mappings.

## Usage

``` r
recode_node_attrs(
  graph,
  node_attr_from,
  ...,
  otherwise = NULL,
  node_attr_to = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr_from:

  The name of the node attribute column from which values will be
  recoded.

- ...:

  Single-length character vectors with the recoding instructions. The
  first component should have the value to replace and the second should
  have the replacement value (in the form
  `"[to_replace] -> [replacement]", ...`).

- otherwise:

  An optional single value for recoding any unmatched values.

- node_attr_to:

  An optional name of a new node attribute to which the recoded values
  will be applied. This will retain the original node attribute and its
  values.

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
[`rename_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_node_attrs.md),
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
    m = 10,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = shape,
    values =
      c("circle", "hexagon",
        "rectangle", "rectangle",
        "circle"))

# Get the graph's internal ndf
# to show which node
# attributes are available
graph |> get_node_df()
#>   id type label     shape
#> 1  1 <NA>     1    circle
#> 2  2 <NA>     2   hexagon
#> 3  3 <NA>     3 rectangle
#> 4  4 <NA>     4 rectangle
#> 5  5 <NA>     5    circle

# Recode the `shape` node
# attribute, so that `circle`
# is recoded to `square` and that
# `rectangle` becomes `triangle`
graph <-
  graph |>
  recode_node_attrs(
    node_attr_from = shape,
    "circle -> square",
    "rectangle -> triangle")

# Get the graph's internal
# ndf to show that the node
# attribute values had been recoded
graph |> get_node_df()
#>   id type label    shape
#> 1  1 <NA>     1   square
#> 2  2 <NA>     2  hexagon
#> 3  3 <NA>     3 triangle
#> 4  4 <NA>     4 triangle
#> 5  5 <NA>     5   square

# Create a new node attribute,
# called `color`, that is based
# on a recoding of `shape`; here,
# map the square shape to a `red`
# color and map all other shapes
# to a `green` color
graph <-
  graph |>
  recode_node_attrs(
    node_attr_from = shape,
    "square -> red",
    otherwise = "green",
    node_attr_to = color)

# Get the graph's internal ndf
# to see the change
graph |> get_node_df()
#>   id type label    shape color
#> 1  1 <NA>     1   square   red
#> 2  2 <NA>     2  hexagon green
#> 3  3 <NA>     3 triangle green
#> 4  4 <NA>     4 triangle green
#> 5  5 <NA>     5   square   red
```
