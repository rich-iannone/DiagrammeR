# Drop an edge attribute column

Within a graph's internal edge data frame (edf), remove an existing edge
attribute.

## Usage

``` r
drop_edge_attrs(graph, edge_attr)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr:

  The name of the edge attribute column to drop.

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
    m = 6,
    set_seed = 23) |>
  set_edge_attrs(
    edge_attr = value,
    values = 3) |>
  mutate_edge_attrs(
    penwidth = value * 2)

# Get the graph's internal
# edf to show which edge
# attributes are available
graph |> get_edge_df()
#>   id from to  rel value penwidth
#> 1  1    1  4 <NA>     3        6
#> 2  2    2  3 <NA>     3        6
#> 3  3    2  4 <NA>     3        6
#> 4  4    3  1 <NA>     3        6
#> 5  5    4  1 <NA>     3        6
#> 6  6    5  4 <NA>     3        6

# Drop the `value` edge
# attribute
graph <-
  graph |>
  drop_edge_attrs(
    edge_attr = value)

# Get the graph's internal
# edf to show that the edge
# attribute `value` had been
# removed
graph |> get_edge_df()
#>   id from to  rel penwidth
#> 1  1    1  4 <NA>        6
#> 2  2    2  3 <NA>        6
#> 3  3    2  4 <NA>        6
#> 4  4    3  1 <NA>        6
#> 5  5    4  1 <NA>        6
#> 6  6    5  4 <NA>        6
```
