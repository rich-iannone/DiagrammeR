# Recode a set of edge attribute values

Within a graph's internal edge data frame (edf), recode character or
numeric edge attribute values. Optionally, one can specify a replacement
value for any unmatched mappings.

## Usage

``` r
recode_edge_attrs(
  graph,
  edge_attr_from,
  ...,
  otherwise = NULL,
  edge_attr_to = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_attr_from:

  The name of the edge attribute column from which values will be
  recoded.

- ...:

  Single-length character vectors with the recoding instructions. The
  first component should have the value to replace and the second should
  have the replacement value (in the form
  `"[to_replace] -> [replacement]", ...`).

- otherwise:

  An optional single value for recoding any unmatched values.

- edge_attr_to:

  An optional name of a new edge attribute to which the recoded values
  will be applied. This will retain the original edge attribute and its
  values.

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
    n = 4,
    m = 6,
    set_seed = 23) |>
  set_edge_attrs(
    edge_attr = rel,
    values = c("a", "b", "a",
               "c", "b", "d"))

# Get the graph's internal edf
# to show which edge attributes
# are available
graph |> get_edge_df()
#>   id from to rel
#> 1  1    1  3   a
#> 2  2    2  4   b
#> 3  3    2  3   a
#> 4  4    3  1   c
#> 5  5    3  4   b
#> 6  6    4  3   d

# Recode the `rel` node
# attribute, creating a new edge
# attribute called `penwidth`;
# here, `a` is recoded to `1.0`,
# `b` maps to `1.5`, and all
# other values become `0.5`
graph <-
  graph |>
  recode_edge_attrs(
    edge_attr_from = rel,
    "a -> 1.0",
    "b -> 1.5",
    otherwise = 0.5,
    edge_attr_to = penwidth)

# Get the graph's internal edf
# to show that the node
# attribute values had been
# recoded and copied into a
# new node attribute
graph |> get_edge_df()
#>   id from to rel penwidth
#> 1  1    1  3   a      1.0
#> 2  2    2  4   b      1.5
#> 3  3    2  3   a      1.0
#> 4  4    3  1   c      0.5
#> 5  5    3  4   b      1.5
#> 6  6    4  3   d      0.5
```
