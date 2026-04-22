# Set the edge attribute values to be rendered

Set a edge attribute type to display as edge text when calling the
[`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md)
function. This allows for display of different types of edge attribute
values on a per-edge basis. Without setting the `display` attribute,
rendering a graph will default to not printing any text on edges.
Setting the `display` edge attribute with this function for the first
time (i.e., the `display` column doesn't exist in the graph's internal
edge data frame) will insert the `attr` value for all edges specified in
`edges` and a default value (`default`) for all remaining edges.

## Usage

``` r
set_edge_attr_to_display(graph, attr = NULL, edges = NULL, default = "label")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- attr:

  The name of the attribute from which label text for the edge will be
  obtained. If set to `NULL`, then `NA` values will be assigned to the
  `display` column for the chosen edges.

- edges:

  A length vector containing one or several edge ID values (as integers)
  for which edge attributes are set for display in the rendered graph.
  If `NULL`, all edges from the graph are assigned the `display` value
  given as `attr`.

- default:

  The name of an attribute to set for all other graph edges not included
  in `edges`. This value only gets used if the `display` edge attribute
  is not in the graph's internal edge data frame.

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
[`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md),
[`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md),
[`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

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
  set_edge_attrs(
    edge_attr = value,
    values = c(2.5, 8.2, 4.2, 2.4))

# For edge ID values of `1`,
# `2`, and `3`, choose to display
# the edge `value` attribute (for
# the other edges, display nothing)
graph <-
  graph |>
  set_edge_attr_to_display(
    edges = 1:3,
    attr = value,
    default = NA)

# Show the graph's edge data frame; the
# `display` edge attribute will show, for
# each row, which edge attribute value to
# display when the graph is rendered
graph |> get_edge_df()
#>   id from to  rel display value
#> 1  1    2  1 <NA>   value   2.5
#> 2  2    3  1 <NA>   value   8.2
#> 3  3    3  2 <NA>   value   4.2
#> 4  4    4  3 <NA>    <NA>   2.4

# This function can be called multiple
# times on a graph; after the first time
# (i.e., creation of the `display`
# attribute), the `default` value won't
# be used
graph |>
  set_edge_attr_to_display(
    edges = 4,
    attr = to) |>
  set_edge_attr_to_display(
    edges = c(1, 3),
    attr = id) |>
  get_edge_df()
#>   id from to  rel display value
#> 1  1    2  1 <NA>      id   2.5
#> 2  2    3  1 <NA>   value   8.2
#> 3  3    3  2 <NA>      id   4.2
#> 4  4    4  3 <NA>      to   2.4
```
