# Mutate a set of edge attribute values

Within a graph's internal edge data frame (edf), mutate numeric edge
attribute values using one or more expressions.

## Usage

``` r
mutate_edge_attrs(graph, ...)
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
# Create a graph with 3 edges
graph <-
  create_graph() |>
  add_path(n = 4) |>
  set_edge_attrs(
    edge_attr = width,
    values = c(3.4, 2.3, 7.2))

# Get the graph's internal edf
# to show which edge attributes
# are available
graph |> get_edge_df()
#>   id from to  rel width
#> 1  1    1  2 <NA>   3.4
#> 2  2    2  3 <NA>   2.3
#> 3  3    3  4 <NA>   7.2

# Mutate the `width` edge
# attribute, dividing each
# value by 2
graph <-
  graph |>
  mutate_edge_attrs(
    width = width / 2)

# Get the graph's internal
# edf to show that the edge
# attribute `width` had its
# values changed
graph |> get_edge_df()
#>   id from to  rel width
#> 1  1    1  2 <NA>  1.70
#> 2  2    2  3 <NA>  1.15
#> 3  3    3  4 <NA>  3.60

# Create a new edge attribute,
# called `length`, that is the
# log of values in `width` plus
# 2 (and, also, round all values
# to 2 decimal places)
graph <-
  graph |>
  mutate_edge_attrs(
    length = (log(width) + 2) |>
               round(2))

# Get the graph's internal edf
# to show that the edge attribute
# values had been mutated
graph |> get_edge_df()
#>   id from to  rel width length
#> 1  1    1  2 <NA>  1.70   2.53
#> 2  2    2  3 <NA>  1.15   2.14
#> 3  3    3  4 <NA>  3.60   3.28

# Create a new edge attribute
# called `area`, which is the
# product of the `width` and
# `length` attributes
graph <-
  graph |>
  mutate_edge_attrs(
    area = width * length)

# Get the graph's internal edf
# to show that the edge attribute
# values had been multiplied
# together (with new attr `area`)
graph |> get_edge_df()
#>   id from to  rel width length   area
#> 1  1    1  2 <NA>  1.70   2.53  4.301
#> 2  2    2  3 <NA>  1.15   2.14  2.461
#> 3  3    3  4 <NA>  3.60   3.28 11.808
```
