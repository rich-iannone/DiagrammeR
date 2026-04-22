# Set the node attribute values to be rendered

Set a node attribute type to display as node text when calling the
[`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md)
function. This allows for display of different types of node attribute
values on a per-node basis. Without setting the `display` attribute,
rendering a graph will default to printing text from the `label`
attribute on nodes. Setting the `display` node attribute with this
function for the first time (i.e., the `display` column doesn't exist in
the graph's internal node data frame) will insert the `attr` value for
all nodes specified in `nodes` and a default value (`default`) for all
remaining nodes.

## Usage

``` r
set_node_attr_to_display(graph, attr = NULL, nodes = NULL, default = "label")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- attr:

  The name of the attribute from which label text for the node will be
  obtained. If set to `NULL`, then `NA` values will be assigned to the
  `display` column for the chosen nodes.

- nodes:

  A length vector containing one or several node ID values (as integers)
  for which node attributes are set for display in the rendered graph.
  If `NULL`, all nodes from the graph are assigned the `display` value
  given as `attr`.

- default:

  The name of an attribute to set for all other graph nodes not included
  in `nodes`. This value only gets used if the `display` node attribute
  is not in the graph's internal node data frame.

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
    n = 4,
    m = 4,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = value,
    values = c(2.5, 8.2, 4.2, 2.4))

# For node ID values of `1`,
# `2`, and `3`, choose to display
# the node `value` attribute (for
# the other nodes, display nothing)
graph <-
  graph |>
  set_node_attr_to_display(
    nodes = 1:3,
    attr = value,
    default = NA)

# Show the graph's node data frame; the
# `display` node attribute will show for
# each row, which node attribute value to
# display when the graph is rendered
graph |> get_node_df()
#>   id type label display value
#> 1  1 <NA>     1   value   2.5
#> 2  2 <NA>     2   value   8.2
#> 3  3 <NA>     3   value   4.2
#> 4  4 <NA>     4    <NA>   2.4

# This function can be called multiple
# times on a graph; after the first time
# (i.e., creation of the `display`
# attribute), the `default` value won't
# be used
graph |>
  set_node_attr_to_display(
    nodes = 4,
    attr = label) |>
  set_node_attr_to_display(
    nodes = c(1, 3),
    attr = id) |>
  get_node_df()
#>   id type label display value
#> 1  1 <NA>     1      id   2.5
#> 2  2 <NA>     2   value   8.2
#> 3  3 <NA>     3      id   4.2
#> 4  4 <NA>     4   label   2.4
```
