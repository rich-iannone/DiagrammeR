# Apply colors based on node attribute values

Within a graph's internal node data frame (ndf), use a categorical node
attribute to generate a new node attribute with color values.

## Usage

``` r
colorize_node_attrs(
  graph,
  node_attr_from,
  node_attr_to,
  cut_points = NULL,
  palette = "Spectral",
  alpha = NULL,
  reverse_palette = FALSE,
  default_color = "#D9D9D9"
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr_from:

  The name of the node attribute column from which color values will be
  based.

- node_attr_to:

  The name of the new node attribute to which the color values will be
  applied.

- cut_points:

  An optional vector of numerical breaks for bucketizing continuous
  numerical values available in a edge attribute column.

- palette:

  Can either be: (1) a palette name from the RColorBrewer package (e.g.,
  `Greens`, `OrRd`, `RdYlGn`), (2) `viridis`, which indicates use of the
  `viridis` color scale from the package of the same name, or (3) a
  vector of hexadecimal color names.

- alpha:

  An optional alpha transparency value to apply to the generated colors.
  Should be in the range of `0` (completely transparent) to `100`
  (completely opaque).

- reverse_palette:

  An option to reverse the order of colors in the chosen palette. The
  default is `FALSE`.

- default_color:

  A hexadecimal color value to use for instances when the values do not
  fall into the bucket ranges specified in the `cut_points` vector.

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
# Create a graph with 8
# nodes and 7 edges
graph <-
  create_graph() |>
  add_path(n = 8) |>
  set_node_attrs(
    node_attr = weight,
    values = c(
      8.2, 3.7, 6.3, 9.2,
      1.6, 2.5, 7.2, 5.4))

# Find group membership values for all nodes
# in the graph through the Walktrap community
# finding algorithm and join those group values
# to the graph's internal node data frame (ndf)
# with the `join_node_attrs()` function
graph <-
  graph |>
  join_node_attrs(
    df = get_cmty_walktrap(graph))

# Inspect the number of distinct communities
graph |>
  get_node_attrs(
    node_attr = walktrap_group) |>
  unique() |>
  sort()
#> [1] 1 2 3

# Visually distinguish the nodes in the different
# communities by applying colors using the
# `colorize_node_attrs()` function; specifically,
# set different `fillcolor` values with an alpha
# value of 90 and apply opaque colors to the node
# border (with the `color` node attribute)
graph <-
  graph |>
  colorize_node_attrs(
    node_attr_from = walktrap_group,
    node_attr_to = fillcolor,
    palette = "Greens",
    alpha = 90) |>
  colorize_node_attrs(
    node_attr_from = walktrap_group,
    node_attr_to = color,
    palette = "viridis",
    alpha = 80)

# Show the graph's internal node data frame
graph |> get_node_df()
#>   id type label weight walktrap_group fillcolor     color
#> 1  1 <NA>     1    8.2              2 #A1D99B90 #21908C80
#> 2  2 <NA>     2    3.7              2 #A1D99B90 #21908C80
#> 3  3 <NA>     3    6.3              2 #A1D99B90 #21908C80
#> 4  4 <NA>     4    9.2              3 #31A35490 #FDE72580
#> 5  5 <NA>     5    1.6              3 #31A35490 #FDE72580
#> 6  6 <NA>     6    2.5              1 #E5F5E090 #44015480
#> 7  7 <NA>     7    7.2              1 #E5F5E090 #44015480
#> 8  8 <NA>     8    5.4              1 #E5F5E090 #44015480

# Create a graph with 8 nodes and 7 edges
graph <-
  create_graph() |>
  add_path(n = 8) |>
  set_node_attrs(
    node_attr = weight,
    values = c(
      8.2, 3.7, 6.3, 9.2,
      1.6, 2.5, 7.2, 5.4))

# We can bucketize values in `weight` using
# `cut_points` and assign colors to each of the
# bucketed ranges (for values not part of any
# bucket, a gray color is assigned by default)
graph <-
  graph |>
  colorize_node_attrs(
    node_attr_from = weight,
    node_attr_to = fillcolor,
    cut_points = c(1, 3, 5, 7, 9))

# Now there will be a `fillcolor` node attribute
# with distinct colors (the `#D9D9D9` color is
# the default `gray85` color)
graph |> get_node_df()
#>   id type label weight fillcolor
#> 1  1 <NA>     1    8.2   #2B83BA
#> 2  2 <NA>     2    3.7   #FDAE61
#> 3  3 <NA>     3    6.3   #ABDDA4
#> 4  4 <NA>     4    9.2   #D9D9D9
#> 5  5 <NA>     5    1.6   #D7191C
#> 6  6 <NA>     6    2.5   #D7191C
#> 7  7 <NA>     7    7.2   #2B83BA
#> 8  8 <NA>     8    5.4   #ABDDA4
```
