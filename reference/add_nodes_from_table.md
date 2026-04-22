# Add nodes and attributes to graph from a table

Add nodes and their attributes to an existing graph object from data in
a CSV file or a data frame.

## Usage

``` r
add_nodes_from_table(
  graph,
  table,
  label_col = NULL,
  type_col = NULL,
  set_type = NULL,
  drop_cols = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- table:

  Either a path to a CSV file, or, a data frame object.

- label_col:

  An option to apply a column of data in the table as `label` attribute
  values.

- type_col:

  An option to apply a column of data in the table as `type` attribute
  values.

- set_type:

  An optional string to apply a `type` attribute to all nodes created
  from data in the external table.

- drop_cols:

  An optional column selection statement for dropping columns from the
  external table before inclusion as attributes in the graph's internal
  node data frame. Several columns can be dropped by name using the
  syntax `col_1 & col_2 & ...`. Columns can also be dropped using a
  numeric column range with `:` (e.g., `5:8`), or, by using the `:`
  between column names to specify the range (e.g.,
  `col_5_name:col_8_name`).

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
[`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md),
[`set_node_attr_w_fcn()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_w_fcn.md),
[`set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.md),
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md),
[`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md)

## Examples

``` r
# To add nodes from the dataset called
# `currencies` (available as a dataset
# in the package), call the
# `add_nodes_from_table()` function
# after creating an empty graph; new
# node ID values will be created as
# monotonically-increasing values
graph_1 <-
  create_graph() |>
  add_nodes_from_table(
    table = currencies)

# View part of the graph's internal
# node data frame (ndf)
ndf_1 <- graph_1 |> get_node_df()

ndf_1[, 1:5] |> head()
#>   id type label iso_4217_code curr_number
#> 1  1 <NA>  <NA>           AED         784
#> 2  2 <NA>  <NA>           AFN         971
#> 3  3 <NA>  <NA>           ALL           8
#> 4  4 <NA>  <NA>           AMD          51
#> 5  5 <NA>  <NA>           ANG         532
#> 6  6 <NA>  <NA>           AOA         973

# If you would like to assign
# any of the table's columns as
# `type` or `label` attributes,
# this can be done with the `type_col`
# and `label_col` arguments; to set
# a static `type` attribute for all
# of the table records, use `set_type`
graph_2 <-
  create_graph() |>
  add_nodes_from_table(
    table = currencies,
    label_col = iso_4217_code,
    set_type = currency)

# View part of the graph's internal ndf
ndf_2 <- graph_2 |> get_node_df()

ndf_2[, 1:5] |> head()
#>   id     type label curr_number exponent
#> 1  1 currency   AED         784        2
#> 2  2 currency   AFN         971        2
#> 3  3 currency   ALL           8        2
#> 4  4 currency   AMD          51        2
#> 5  5 currency   ANG         532        2
#> 6  6 currency   AOA         973        2

# Suppose we would like to not
# include certain columns from the
# external table in the resulting
# graph; we can use the `drop_cols`
# argument to choose which columns
# to not include as attributes
graph_3 <-
  create_graph() |>
  add_nodes_from_table(
    table = currencies,
    label_col = iso_4217_code,
    set_type = currency,
    drop_cols = exponent & currency_name)

# Show the node attribute names
# for the graph; note that the
# `exponent` and `currency_name`
# columns are not attributes in the
# graph's internal node data frame
graph_3 |>
  get_node_df() |>
  colnames()
#> [1] "id"          "type"        "label"       "curr_number"
```
