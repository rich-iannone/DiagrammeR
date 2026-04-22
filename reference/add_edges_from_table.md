# Add edges and attributes to graph from a table

Add edges and their attributes to an existing graph object from data in
a CSV file or a data frame.

## Usage

``` r
add_edges_from_table(
  graph,
  table,
  from_col,
  to_col,
  from_to_map,
  rel_col = NULL,
  set_rel = NULL,
  drop_cols = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- table:

  Either a path to a CSV file, or, a data frame object.

- from_col:

  The name of the table column from which edges originate.

- to_col:

  The name of the table column to which edges terminate.

- from_to_map:

  A single character value for the mapping of the `from` and `to`
  columns in the external table (supplied as `from_col` and `to_col`,
  respectively) to a column in the graph's internal node data frame
  (ndf).

- rel_col:

  An option to apply a column of data in the table as `rel` attribute
  values.

- set_rel:

  an optional string to apply a `rel` attribute to all edges created
  from the table records.

- drop_cols:

  An optional column selection statement for dropping columns from the
  external table before inclusion as attributes in the graph's internal
  edge data frame. Several columns can be dropped by name using the
  syntax `col_1 & col_2 & ...`. Columns can also be dropped using a
  numeric column range with `:` (e.g., `5:8`), or, by using the `:`
  between column names to specify the range (e.g.,
  `col_5_name:col_8_name`).

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
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
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create an empty graph and then
# add nodes to it from the
# `currencies` dataset available
# in the package
graph <-
  create_graph() |>
  add_nodes_from_table(
    table = currencies)

# Now we want to add edges to the
# graph using an included dataset,
# `usd_exchange_rates`, which has
# exchange rates between USD and
# many other currencies; the key
# here is that the data in the
# `from` and `to` columns in the
# external table maps to graph
# node data available in the
# `iso_4217_code` column of the
# graph's internal node data frame
graph_1 <-
  graph |>
    add_edges_from_table(
      table = usd_exchange_rates,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code)

# View part of the graph's
# internal edge data frame
graph_1 |>
  get_edge_df() |>
  head()
#>   id from to  rel cost_unit
#> 1  1  148  1 <NA>  0.272300
#> 2  2  148  2 <NA>  0.015210
#> 3  3  148  3 <NA>  0.008055
#> 4  4  148  4 <NA>  0.002107
#> 5  5  148  5 <NA>  0.565000
#> 6  6  148  6 <NA>  0.006058

# If you would like to assign
# any of the table's columns as the
# `rel` attribute, this can done
# with the `rel_col` argument; to
# set a static `rel` attribute for
# all edges created, use `set_rel`
graph_2 <-
  graph |>
    add_edges_from_table(
      table = usd_exchange_rates,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code,
      set_rel = "from_usd")

# View part of the graph's internal
# edge data frame (edf)
graph_2 |>
  get_edge_df() |>
  head()
#>   id from to      rel cost_unit
#> 1  1  148  1 from_usd  0.272300
#> 2  2  148  2 from_usd  0.015210
#> 3  3  148  3 from_usd  0.008055
#> 4  4  148  4 from_usd  0.002107
#> 5  5  148  5 from_usd  0.565000
#> 6  6  148  6 from_usd  0.006058
```
