# Select nodes in the graph based on their degree values

Using a graph object of class `dgr_graph`, create a selection of nodes
that have certain degree values.

## Usage

``` r
select_nodes_by_degree(graph, expressions, set_op = "union")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- expressions:

  One or more expressions for filtering nodes by degree values. Use a
  combination of degree type (`deg` for total degree, `indeg` for
  in-degree, and `outdeg` for out-degree) with a comparison operator and
  values for comparison (e.g., use `"deg >= 2"` to select nodes with a
  degree greater than or equal to 2).

- set_op:

  The set operation to perform upon consecutive selections of graph
  nodes. This can either be as a `union` (the default), as an
  intersection of selections with `intersect`, or, as a `difference` on
  the previous selection, if it exists.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a random graph using
# the `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 35, m = 125,
    set_seed = 23)

# Report which nodes have a
# total degree (in-degree +
# out-degree) of exactly 9
graph |>
  select_nodes_by_degree(
    expressions = "deg == 9") |>
  get_selection()
#> [1]  5 10 26 31

# Report which nodes have a
# total degree greater than or
# equal to 9
graph |>
  select_nodes_by_degree(
    expressions = "deg >= 9") |>
  get_selection()
#>  [1]  1  2  4  5 10 12 18 25 26 31

# Combine two calls of
# `select_nodes_by_degree()` to
# get those nodes with total
# degree less than 3 and total
# degree greater than 10 (by
# default, those `select...()`
# functions will `union` the
# sets of nodes selected)
graph |>
  select_nodes_by_degree(
    expressions = "deg < 3") |>
  select_nodes_by_degree(
    expressions = "deg > 10") |>
  get_selection()
#> [1] 1 2

# Combine two calls of
# `select_nodes_by_degree()` to
# get those nodes with total
# degree greater than or equal
# to 3 and less than or equal
# to 10 (the key here is to
# `intersect` the sets of nodes
# selected in the second call)
graph |>
  select_nodes_by_degree(
    expressions = "deg >= 3") |>
  select_nodes_by_degree(
    expressions = "deg <= 10",
    set_op = "intersect") |>
  get_selection()
#>  [1]  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
#> [26] 28 29 30 31 32 33 34 35

# Select all nodes with an
# in-degree greater than 5, then,
# apply a node attribute to those
# selected nodes (coloring the
# selected nodes red)
graph_2 <-
  graph |>
  select_nodes_by_degree(
    expressions = "indeg > 5") |>
  set_node_attrs_ws(
    node_attr = color,
    value = "red")

# Get the selection of nodes
graph_2 |> get_selection()
#> [1]  1  4 13 25 35
```
