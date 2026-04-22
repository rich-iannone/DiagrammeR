# Mutate edge attribute values for a selection of edges

Within a graph's internal edge data frame (edf), mutate edge attribute
values only for edges in a selection by using one or more expressions.

This function makes use of an active selection of edges (and the
function ending with `_ws` hints at this).

Selections of edges can be performed using the following selection
(`select_*()`) functions:
[`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md),
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md),
[`select_edges_by_edge_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_edge_id.md),
or
[`select_edges_by_node_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_node_id.md).

Selections of edges can also be performed using the following traversal
(`trav_*()`) functions:
[`trav_out_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_edge.md),
[`trav_in_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_edge.md),
[`trav_both_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both_edge.md),
or
[`trav_reverse_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_reverse_edge.md).

## Usage

``` r
mutate_edge_attrs_ws(graph, ...)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- ...:

  Expressions used for the mutation of edge attributes. LHS of each
  expression is either an existing or new edge attribute name. The RHS
  can consist of any valid R code that uses edge attributes as
  variables. Expressions are evaluated in the order provided, so, edge
  attributes created or modified are ready to use in subsequent
  expressions.

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
# Create a graph with 3 edges
# and then select edge `1`
graph <-
  create_graph() |>
  add_path(n = 4) |>
  set_edge_attrs(
    edge_attr = width,
    values = c(3.4, 2.3, 7.2)) |>
  select_edges(edges = 1)

# Get the graph's internal edf
# to show which edge attributes
# are available
graph |> get_edge_df()
#>   id from to  rel width
#> 1  1    1  2 <NA>   3.4
#> 2  2    2  3 <NA>   2.3
#> 3  3    3  4 <NA>   7.2

# Mutate the `width` edge
# attribute for the edges
# only in the active selection
# of edges (edge `1`); here,
# we divide each value in the
# selection by 2
graph <-
  graph |>
  mutate_edge_attrs_ws(
    width = width / 2)

# Get the graph's internal
# edf to show that the edge
# attribute `width` had its
# values changed
graph |> get_edge_df()
#>   id from to  rel width
#> 1  1    1  2 <NA>   1.7
#> 2  2    2  3 <NA>   2.3
#> 3  3    3  4 <NA>   7.2

# Create a new edge attribute,
# called `length`, that is the
# log of values in `width` plus
# 2 (and, also, round all values
# to 2 decimal places)
graph <-
  graph |>
  clear_selection() |>
  select_edges(edges = 2:3) |>
  mutate_edge_attrs_ws(
    length = (log(width) + 2) |>
               round(2))

# Get the graph's internal edf
# to show that the edge attribute
# values had been mutated only
# for edges `2` and `3` (since
# edge `1` is excluded, an NA
# value is applied)
graph |> get_edge_df()
#>   id from to  rel width length
#> 1  1    1  2 <NA>   1.7     NA
#> 2  2    2  3 <NA>   2.3   2.83
#> 3  3    3  4 <NA>   7.2   3.97

# Create a new edge attribute
# called `area`, which is the
# product of the `width` and
# `length` attributes
graph <-
  graph |>
  mutate_edge_attrs_ws(
    area = width * length)

# Get the graph's internal edf
# to show that the edge attribute
# values had been multiplied
# together (with new attr `area`)
# for nodes `2` and `3`
graph |> get_edge_df()
#>   id from to  rel width length   area
#> 1  1    1  2 <NA>   1.7     NA     NA
#> 2  2    2  3 <NA>   2.3   2.83  6.509
#> 3  3    3  4 <NA>   7.2   3.97 28.584

# We can invert the selection
# and mutate edge `1` several
# times to get an `area` value
# for that edge
graph <-
  graph |>
  invert_selection() |>
  mutate_edge_attrs_ws(
    length = (log(width) + 5) |>
               round(2),
    area = width * length)

# Get the graph's internal edf
# to show that the 2 mutations
# occurred for edge `1`, yielding
# non-NA values for its edge
# attributes without changing
# those of the other edges
graph |> get_edge_df()
#>   id from to  rel width length   area
#> 1  1    1  2 <NA>   1.7   5.53  9.401
#> 2  2    2  3 <NA>   2.3   2.83  6.509
#> 3  3    3  4 <NA>   7.2   3.97 28.584
```
