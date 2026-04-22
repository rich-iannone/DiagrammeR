# Copy an edge attribute column and set the name

Within a graph's internal edge data frame (edf), copy the contents an
existing edge attribute and create a distinct edge attribute within the
edf with a different attribute name.

## Usage

``` r
copy_edge_attrs(graph, edge_attr_from, edge_attr_to)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr_from:

  The name of the edge attribute column from which values will be
  copied.

- edge_attr_to:

  The name of the new edge attribute column to which the copied values
  will be placed.

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
[`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md),
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
    n = 5,
    m = 8,
    set_seed = 23) |>
  set_edge_attrs(
    edge_attr = color,
    values = "green")

# Get the graph's internal
# edf to show which edge
# attributes are available
graph |> get_edge_df()
#>   id from to  rel color
#> 1  1    1  3 <NA> green
#> 2  2    2  1 <NA> green
#> 3  3    2  5 <NA> green
#> 4  4    2  3 <NA> green
#> 5  5    3  2 <NA> green
#> 6  6    3  5 <NA> green
#> 7  7    3  4 <NA> green
#> 8  8    5  3 <NA> green

# Make a copy the `color`
# edge attribute as the
# `color_2` edge attribute
graph <-
  graph |>
  copy_edge_attrs(
    edge_attr_from = color,
    edge_attr_to = color_2)

# Get the graph's internal
# edf to show that the edge
# attribute had been copied
graph |> get_edge_df()
#>   id from to  rel color color_2
#> 1  1    1  3 <NA> green   green
#> 2  2    2  1 <NA> green   green
#> 3  3    2  5 <NA> green   green
#> 4  4    2  3 <NA> green   green
#> 5  5    3  2 <NA> green   green
#> 6  6    3  5 <NA> green   green
#> 7  7    3  4 <NA> green   green
#> 8  8    5  3 <NA> green   green
```
