# Add nodes from distinct values in data frame columns

Add new nodes to a graph object of class `dgr_graph` using distinct
values from one or more columns in a data frame. The values will serve
as node labels and the number of nodes added depends on the number of
distinct values found in the specified columns.

## Usage

``` r
add_nodes_from_df_cols(
  graph,
  df,
  columns,
  type = NULL,
  keep_duplicates = FALSE
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- df:

  A data frame from which values will be taken as new nodes for the
  graph.

- columns:

  A character vector of column names or a numeric vector of column
  numbers for the data frame supplied in `df`. The distinct values in
  these columns will serve as labels for the nodes added to the graph.

- type:

  An optional, single-length character vector that provides a group
  identifier for the nodes to be added to the graph.

- keep_duplicates:

  An option to exclude incoming nodes where the labels (i.e., values
  found in columns of the specified `df`) match label values available
  in the graph's nodes. By default, this is set to `FALSE`.

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
[`add_nodes_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_nodes_from_table.md),
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
# Create an empty graph
graph <- create_graph()

# Create a data frame from
# which several columns have
# values designated as graph nodes
df <-
  data.frame(
    col_1 = c("f", "p", "q"),
    col_2 = c("q", "x", "f"),
    col_3 = c(1, 5, 3),
    col_4 = c("a", "v", "h"),
    stringsAsFactors = FALSE)

# Add nodes from columns `col_1`
# and `col_2` from the data frame
# to the graph object
graph <-
  graph |>
  add_nodes_from_df_cols(
    df = df,
    columns = c("col_1", "col_2"))

# Show the graph's node data
# frame; duplicate labels are
# prevented with `keep_duplicates =
# FALSE`)
graph |> get_node_df()
#>   id type label
#> 1  1 <NA>     f
#> 2  2 <NA>     p
#> 3  3 <NA>     q
#> 4  4 <NA>     x

# Add new nodes from columns 3 and 4;
# We can specify the columns by their
# numbers as well
graph <-
  graph |>
  add_nodes_from_df_cols(
    df = df,
    columns = 3:4)

# Show the graph's node data
# frame; note that nodes didn't
# get made with columns that
# are not character class columns
graph |> get_node_df()
#>   id type label
#> 1  1 <NA>     f
#> 2  2 <NA>     p
#> 3  3 <NA>     q
#> 4  4 <NA>     x
#> 5  5 <NA>     a
#> 6  6 <NA>     v
#> 7  7 <NA>     h
```
