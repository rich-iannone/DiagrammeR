# Set node attribute values with a graph function

From a graph object of class `dgr_graph` or a node data frame, set node
attribute properties for all nodes in the graph using one of several
whole-graph functions.

## Usage

``` r
set_node_attr_w_fcn(graph, node_attr_fcn, ..., column_name = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr_fcn:

  The name of the function to use for creating a column of node
  attribute values. Valid functions are:
  [`get_alpha_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_alpha_centrality.md),
  [`get_authority_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_authority_centrality.md),
  [`get_betweenness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_betweenness.md),
  [`get_closeness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_closeness.md),
  [`get_cmty_edge_btwns()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_edge_btwns.md),
  [`get_cmty_fast_greedy()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_fast_greedy.md),
  [`get_cmty_l_eigenvec()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_l_eigenvec.md),
  [`get_cmty_louvain()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_louvain.md),
  [`get_cmty_walktrap()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_walktrap.md),
  [`get_degree_distribution()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_distribution.md),
  [`get_degree_histogram()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_histogram.md),
  [`get_degree_in()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_in.md),
  [`get_degree_out()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_out.md),
  [`get_degree_total()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_total.md),
  [`get_eccentricity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_eccentricity.md),
  [`get_eigen_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_eigen_centrality.md),
  [`get_pagerank()`](https://rich-iannone.github.io/DiagrammeR/reference/get_pagerank.md),
  [`get_s_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_s_connected_cmpts.md),
  and
  [`get_w_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_w_connected_cmpts.md).

- ...:

  Arguments and values to pass to the named function in `node_attr_fcn`,
  if necessary.

- column_name:

  An option to supply a column name for the new node attribute column.
  If `NULL` then the column name supplied by the function will used
  along with a `__A` suffix.

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
[`join_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_node_attrs.md),
[`layout_nodes_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/layout_nodes_w_string.md),
[`mutate_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs.md),
[`mutate_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs_ws.md),
[`node_data()`](https://rich-iannone.github.io/DiagrammeR/reference/node_data.md),
[`recode_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_node_attrs.md),
[`rename_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_node_attrs.md),
[`rescale_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_node_attrs.md),
[`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md),
[`set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.md),
[`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md),
[`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md)

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 22,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = value,
    values = rnorm(
      n = 10,
      mean = 5,
      sd = 1) |> round(1))

# Get the betweenness values for
# each of the graph's nodes as a
# node attribute
graph_1 <-
  graph |>
  set_node_attr_w_fcn(
    node_attr_fcn = "get_betweenness")

# Inspect the graph's internal
# node data frame
graph_1 |> get_node_df()
#>    id type label value betweenness__A
#> 1   1 <NA>     1   4.4       9.333333
#> 2   2 <NA>     2   4.6      29.000000
#> 3   3 <NA>     3   5.9      19.166667
#> 4   4 <NA>     4   6.5       2.666667
#> 5   5 <NA>     5   4.1       0.500000
#> 6   6 <NA>     6   3.6      18.000000
#> 7   7 <NA>     7   5.4      12.000000
#> 8   8 <NA>     8   5.8       0.000000
#> 9   9 <NA>     9   4.7      10.333333
#> 10 10 <NA>    10   5.7       0.000000

# If a specified function takes argument
# values, these can be supplied as well
graph_2 <-
  graph |>
  set_node_attr_w_fcn(
    node_attr_fcn = "get_alpha_centrality",
    alpha = 2,
    exo = 2)

# Inspect the graph's internal
# node data frame
graph_2 |> get_node_df()
#>    id type label value alpha_centrality__A
#> 1   1 <NA>     1   4.4           0.0621118
#> 2   2 <NA>     2   4.6          -0.5341615
#> 3   3 <NA>     3   5.9          -0.8157350
#> 4   4 <NA>     4   6.5          -0.6997930
#> 5   5 <NA>     5   4.1           1.0641822
#> 6   6 <NA>     6   3.6          -0.8737060
#> 7   7 <NA>     7   5.4          -0.6832298
#> 8   8 <NA>     8   5.8           0.9316770
#> 9   9 <NA>     9   4.7          -0.4679089
#> 10 10 <NA>    10   5.7           0.3685300

# The new column name can be provided
graph_3 <-
  graph |>
  set_node_attr_w_fcn(
    node_attr_fcn = "get_pagerank",
    column_name = "pagerank")

# Inspect the graph's internal
# node data frame
graph_3 |> get_node_df()
#>    id type label value pagerank
#> 1   1 <NA>     1   4.4   0.1416
#> 2   2 <NA>     2   4.6   0.1401
#> 3   3 <NA>     3   5.9   0.1262
#> 4   4 <NA>     4   6.5   0.0637
#> 5   5 <NA>     5   4.1   0.0478
#> 6   6 <NA>     6   3.6   0.1976
#> 7   7 <NA>     7   5.4   0.1318
#> 8   8 <NA>     8   5.8   0.0422
#> 9   9 <NA>     9   4.7   0.0693
#> 10 10 <NA>    10   5.7   0.0398

# If `graph_3` is modified by
# adding a new node then the column
# `pagerank` will have stale data; we
# can run the function again and re-use
# the existing column name to provide
# updated values
graph_3 <-
  graph_3 |>
  add_node(
    from = 1,
    to = 3) |>
  set_node_attr_w_fcn(
    node_attr_fcn = "get_pagerank",
    column_name = "pagerank")

# Inspect the graph's internal
# node data frame
graph_3 |> get_node_df()
#>    id type label value pagerank
#> 1   1 <NA>     1   4.4   0.1349
#> 2   2 <NA>     2   4.6   0.1352
#> 3   3 <NA>     3   5.9   0.1585
#> 4   4 <NA>     4   6.5   0.0670
#> 5   5 <NA>     5   4.1   0.0461
#> 6   6 <NA>     6   3.6   0.1300
#> 7   7 <NA>     7   5.4   0.1014
#> 8   8 <NA>     8   5.8   0.0400
#> 9   9 <NA>     9   4.7   0.0685
#> 10 10 <NA>    10   5.7   0.0440
#> 11 11 <NA>  <NA>    NA   0.0744
```
