# Rescale numeric node attribute values

From a graph object of class `dgr_graph`, take a set of numeric values
for a node attribute, rescale to a new numeric or color range, then
write to the same node attribute or to a new node attribute column.

## Usage

``` r
rescale_node_attrs(
  graph,
  node_attr_from,
  to_lower_bound = 0,
  to_upper_bound = 1,
  node_attr_to = NULL,
  from_lower_bound = NULL,
  from_upper_bound = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr_from:

  The node attribute containing numeric data that is to be rescaled to
  new numeric or color values.

- to_lower_bound:

  The lower bound value for the set of rescaled values. This can be a
  numeric value or an X11 color name.

- to_upper_bound:

  The upper bound value for the set of rescaled values. This can be a
  numeric value or an X11 color name.

- node_attr_to:

  An optional name of a new node attribute to which the recoded values
  will be applied. This will retain the original node attribute and its
  values.

- from_lower_bound:

  An optional, manually set lower bound value for the rescaled values.
  If not set, the minimum value from the set will be used.

- from_upper_bound:

  An optional, manually set upper bound value for the rescaled values.
  If not set, the minimum value from the set will be used.

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
    node_attr = value,
    values = rnorm(
      n = 5,
      mean = 5,
      sd = 1) |> round(1))

# Get the graph's internal ndf
# to show which node attributes
# are available
graph |> get_node_df()
#>   id type label value
#> 1  1 <NA>     1   5.3
#> 2  2 <NA>     2   4.4
#> 3  3 <NA>     3   5.8
#> 4  4 <NA>     4   5.9
#> 5  5 <NA>     5   6.2

# Rescale the `value` node
# attribute, so that its values
# are rescaled between 0 and 1
graph <-
  graph |>
  rescale_node_attrs(
    node_attr_from = value,
    to_lower_bound = 0,
    to_upper_bound = 1)

# Get the graph's internal ndf
# to show that the node attribute
# values had been rescaled
graph |> get_node_df()
#>   id type label value
#> 1  1 <NA>     1 0.500
#> 2  2 <NA>     2 0.000
#> 3  3 <NA>     3 0.778
#> 4  4 <NA>     4 0.833
#> 5  5 <NA>     5 1.000

# Scale the values in the `value`
# node attribute to different
# shades of gray for the `fillcolor`
# and `fontcolor` node attributes
graph <-
  graph |>
  rescale_node_attrs(
    node_attr_from = value,
    to_lower_bound = "gray80",
    to_upper_bound = "gray20",
    node_attr_to = fillcolor) |>
  rescale_node_attrs(
    node_attr_from = value,
    to_lower_bound = "gray5",
    to_upper_bound = "gray95",
    node_attr_to = fontcolor)

# Get the graph's internal ndf
# once more to show that scaled
# grayscale colors are now available
# in the `fillcolor` and `fontcolor`
# node attributes
graph |> get_node_df()
#>   id type label value fillcolor fontcolor
#> 1  1 <NA>     1 0.500   #7B7B7B   #767676
#> 2  2 <NA>     2 0.000   #CCCCCC   #0D0D0D
#> 3  3 <NA>     3 0.778   #525252   #B9B9B9
#> 4  4 <NA>     4 0.833   #4A4A4A   #C7C7C7
#> 5  5 <NA>     5 1.000   #333333   #F2F2F2
```
