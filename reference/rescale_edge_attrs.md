# Rescale numeric edge attribute values

From a graph object of class `dgr_graph`, take a set of numeric values
for an edge attribute, rescale to a new numeric or color range, then
write to the same edge attribute or to a new edge attribute column.

## Usage

``` r
rescale_edge_attrs(
  graph,
  edge_attr_from,
  to_lower_bound = 0,
  to_upper_bound = 1,
  edge_attr_to = NULL,
  from_lower_bound = NULL,
  from_upper_bound = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr_from:

  The edge attribute containing numeric data that is to be rescaled to
  new numeric or color values.

- to_lower_bound:

  The lower bound value for the set of rescaled values. This can be a
  numeric value or an X11 color name.

- to_upper_bound:

  The upper bound value for the set of rescaled values. This can be a
  numeric value or an X11 color name.

- edge_attr_to:

  An optional name of a new edge attribute to which the recoded values
  will be applied. This will retain the original edge attribute and its
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

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
[`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md),
[`add_edges_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_w_string.md),
[`add_forward_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_forward_edges_ws.md),
[`add_reverse_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_reverse_edges_ws.md),
[`copy_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_edge_attrs.md),
[`create_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_edge_df.md),
[`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md),
[`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md),
[`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md),
[`drop_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_edge_attrs.md),
[`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md),
[`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md),
[`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md),
[`mutate_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs_ws.md),
[`recode_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_edge_attrs.md),
[`rename_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_edge_attrs.md),
[`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md),
[`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md),
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 7,
    set_seed = 23) |>
  set_edge_attrs(
    edge_attr = weight,
    values = rnorm(
      n = 7,
      mean = 5,
      sd = 1))

# Get the graph's internal edf
# to show which edge attributes
# are available
graph |> get_edge_df()
#>   id from to  rel   weight
#> 1  1    2  8 <NA> 5.045437
#> 2  2    4  2 <NA> 6.575780
#> 3  3    4  6 <NA> 5.218288
#> 4  4    4  9 <NA> 3.953465
#> 5  5    6  5 <NA> 4.711311
#> 6  6    6 10 <NA> 5.481550
#> 7  7   10  9 <NA> 3.783624

# Rescale the `weight` edge
# attribute, so that its values
# are rescaled between 0 and 1
graph <-
  graph |>
  rescale_edge_attrs(
    edge_attr_from = weight,
    to_lower_bound = 0,
    to_upper_bound = 1)

# Get the graph's internal edf
# to show that the edge attribute
# values had been rescaled
graph |> get_edge_df()
#>   id from to  rel weight
#> 1  1    2  8 <NA>  0.452
#> 2  2    4  2 <NA>  1.000
#> 3  3    4  6 <NA>  0.514
#> 4  4    4  9 <NA>  0.061
#> 5  5    6  5 <NA>  0.332
#> 6  6    6 10 <NA>  0.608
#> 7  7   10  9 <NA>  0.000

# Scale the values in the `weight`
# edge attribute to different
# shades of gray for the `color`
# edge attribute and different
# numerical values for the
# `penwidth` attribute
graph <-
  graph |>
  rescale_edge_attrs(
    edge_attr_from = weight,
    to_lower_bound = "gray80",
    to_upper_bound = "gray20",
    edge_attr_to = color) |>
  rescale_edge_attrs(
    edge_attr_from = weight,
    to_lower_bound = 0.5,
    to_upper_bound = 3,
    edge_attr_to = penwidth)

# Get the graph's internal edf
# once more to show that scaled
# grayscale colors are now available
# in `color` and scaled numerical
# values are in the `penwidth`
# edge attribute
graph |> get_edge_df()
#>   id from to  rel weight   color penwidth
#> 1  1    2  8 <NA>  0.452 #838383    1.630
#> 2  2    4  2 <NA>  1.000 #333333    3.000
#> 3  3    4  6 <NA>  0.514 #797979    1.785
#> 4  4    4  9 <NA>  0.061 #C2C2C2    0.652
#> 5  5    6  5 <NA>  0.332 #959595    1.330
#> 6  6    6 10 <NA>  0.608 #6B6B6B    2.020
#> 7  7   10  9 <NA>  0.000 #CCCCCC    0.500
```
