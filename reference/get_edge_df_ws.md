# Get the graph's edf filtered by a selection of edges

From a graph object of class `dgr_graph`, get the graph's internal edge
data frame that is filtered by the edge ID values currently active as a
selection.

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
get_edge_df_ws(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

an edge data frame.

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

# Select edges with ID values
# `1` and `3`
graph <-
  graph |>
  select_edges_by_edge_id(
    edges = c(1, 3))

# Get the edge data frame that's
# limited to the rows that correspond
# to the edge selection
graph |> get_edge_df_ws()
#>   id from to  rel value
#> 1  1    2  1 <NA>   2.5
#> 2  3    3  2 <NA>   4.2
```
