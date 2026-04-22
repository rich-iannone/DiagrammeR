# Join new node attribute values using a data frame

Join new node attribute values in a left join using a data frame. The
use of a left join in this function allows for no possibility that nodes
in the graph might be removed after the join.

## Usage

``` r
join_node_attrs(graph, df, by_graph = NULL, by_df = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- df:

  The data frame to use for joining.

- by_graph:

  Optional specification of the column in the graph's internal node data
  frame for the left join. If both `by_graph` and `by_df` are not
  provided, then a natural join will occur if there are columns in the
  graph's ndf and in `df` with identical names.

- by_df:

  Optional specification of the column in `df` for the left join. If
  both `by_graph` and `by_df` are not provided, then a natural join will
  occur if there are columns in the graph's ndf and in `df` with
  identical names.

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
[`colorize_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_node_attrs.md),
[`copy_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_node_attrs.md),
[`create_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_node_df.md),
[`delete_node()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_node.md),
[`delete_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_nodes_ws.md),
[`drop_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_node_attrs.md),
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
# Set a seed
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a simple graph
graph <-
  create_graph() |>
  add_n_nodes(n = 5) |>
  add_edges_w_string(
    edges = "1->2 1->3 2->4 2->5 3->5")

# Create a data frame with node ID values and a
# set of numeric values
df <-
  data.frame(
    values = round(rnorm(6, 5), 2),
    id = 1:6)

# Join the values in the data frame to the
# graph's nodes; this works as a left join using
# identically-named columns in the graph and the df
# (in this case the `id` column is common to both)
graph <-
  graph |>
  join_node_attrs(
    df = df)

# Get the graph's internal ndf to show that the
# join has been made
graph |> get_node_df()
#>   id type label values
#> 1  1 <NA>  <NA>   6.00
#> 2  2 <NA>  <NA>   6.11
#> 3  3 <NA>  <NA>   4.72
#> 4  4 <NA>  <NA>   6.02
#> 5  5 <NA>  <NA>   5.05

# Get betweenness values for each node and
# add them as a node attribute (Note the
# common column name `id` in the different
# tables results in a natural join)
graph <-
  graph |>
  join_node_attrs(
    df = get_betweenness(graph))

# Get the graph's internal ndf to show that
# this join has been made
graph |> get_node_df()
#>   id type label values betweenness
#> 1  1 <NA>  <NA>   6.00         0.0
#> 2  2 <NA>  <NA>   6.11         1.5
#> 3  3 <NA>  <NA>   4.72         0.5
#> 4  4 <NA>  <NA>   6.02         0.0
#> 5  5 <NA>  <NA>   5.05         0.0
```
