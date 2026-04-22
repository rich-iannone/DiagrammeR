# Apply colors based on edge attribute values

Within a graph's internal edge data frame (edf), use a categorical edge
attribute to generate a new edge attribute with color values.

## Usage

``` r
colorize_edge_attrs(
  graph,
  edge_attr_from,
  edge_attr_to,
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

- edge_attr_from:

  The name of the edge attribute column from which color values will be
  based.

- edge_attr_to:

  The name of the new edge attribute to which the color values will be
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

## Examples

``` r
# Create a graph with 5
# nodes and 4 edges
graph <-
  create_graph() |>
  add_path(n = 5) |>
  set_edge_attrs(
    edge_attr = weight,
    values = c(3.7, 6.3, 9.2, 1.6))

# We can bucketize values in
# the edge `weight` attribute using
# `cut_points` and, by doing so,
# assign colors to each of the
# bucketed ranges (for values not
# part of any bucket, a gray color
# is assigned by default)
graph <-
  graph |>
  colorize_edge_attrs(
    edge_attr_from = weight,
    edge_attr_to = color,
    cut_points = c(0, 2, 4, 6, 8, 10),
    palette = "RdYlGn")

# Now there will be a `color`
# edge attribute with distinct
# colors (from the RColorBrewer
# Red-Yellow-Green palette)
graph |> get_edge_df()
#>   id from to  rel weight   color
#> 1  1    1  2 <NA>    3.7 #FDAE61
#> 2  2    2  3 <NA>    6.3 #A6D96A
#> 3  3    3  4 <NA>    9.2 #1A9641
#> 4  4    4  5 <NA>    1.6 #D7191C
```
