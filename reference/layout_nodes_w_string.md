# Layout nodes using a text-based schematic

Layout one or several groups of nodes using a text-based schematic. The
option is available to apply sorting to each of the groups.

## Usage

``` r
layout_nodes_w_string(
  graph,
  layout,
  nodes,
  sort = NULL,
  width = 8,
  height = 8,
  ll = c(0, 0)
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- layout:

  A layout character string that provides a schematic for the layout.
  This consists of a rectangular collection of `-` characters (for no
  node placement), and numbers from `1` to `9` (representing different
  groupings of nodes, further described in the `nodes` argument).

- nodes:

  A named vector of the form: `c("1" = "[node_attr]:[value]", ...)`. The
  LHS corresponds to the numbers used in the `layout` schematic. The RHS
  provides a shorthand for the node attribute and a value for grouping
  together nodes (separated by a colon). For instance, with `"type:a"`
  in the RHS (and `"1"` in the LHS) we would target all nodes with a
  `type` attribute equal to `a` for positioning in the graph as
  described by the `1`s in the `layout`.

- sort:

  An optional sorting method to apply to the collection of nodes before
  assigning positional information. Like `nodes`, this is a named vector
  of the form: `c("1" = "[node_attr]:asc|desc", ...)`. The `node_attr`
  in this case should be different than that used in `nodes`. Ideally,
  this node attribute should have unique values. Choose either `asc` or
  `desc` right of the colon for ascending or descending sorts.

- width:

  The width of the `layout` diagram.

- height:

  The height of the `layout` diagram.

- ll:

  A vector describing the the lower-left coordinates of the `layout`

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
# Create a graph with unique labels and
# several node `type` groupings
graph <-
  create_graph() |>
  add_node(type = "a", label = "a") |>
  add_node(type = "a", label = "b") |>
  add_node(type = "b", label = "c") |>
  add_node(type = "b", label = "d") |>
  add_node(type = "b", label = "e") |>
  add_node(type = "c", label = "f") |>
  add_node(type = "c", label = "g")

# Define a 'layout' for groups of nodes
# using a text string (dashes are empty
# grid cells, numbers--representing
# ad-hoc groupings--correspond to
# individual nodes); here, define a layout
# with 3 groups of nodes
layout <-
"
1--------
1--------
---222---
--------3
--------3
"

# Use the `layout` along with what nodes
# the numbers correspond to in the graph
# with the `nodes` named vectors; the
# optional `sort` vector describes how
# we should sort the collection of node
# before adding position information
graph <-
  graph |>
  layout_nodes_w_string(
    layout = layout,
    nodes = c("1" = "type:a",
              "2" = "type:b",
              "3" = "type:c"),
    sort = c("1" = "label:asc",
             "2" = "label:desc",
             "3" = "label:desc"))

# Show the graph's node data frame
# to confirm that `x` and `y` values
# were added to each of the nodes
graph |> get_node_df()
#>   id type label x y
#> 1  1    a     a 0 8
#> 2  2    a     b 0 6
#> 3  3    b     c 5 4
#> 4  4    b     d 4 4
#> 5  5    b     e 3 4
#> 6  6    c     f 8 0
#> 7  7    c     g 8 2
```
