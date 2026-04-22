# Traverse from one or more selected nodes onto adjacent edges

From a graph object of class `dgr_graph` move to adjacent edges from a
selection of one or more selected nodes, thereby creating a selection of
edges. An optional filter by edge attribute can limit the set of edges
traversed to.

This traversal function makes use of an active selection of nodes. After
the traversal, depending on the traversal conditions, there will either
be a selection of edges or no selection at all.

Selections of nodes can be performed using the following node selection
(`select_*()`) functions:
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md),
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md),
[`select_nodes_by_degree()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_degree.md),
[`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md),
or
[`select_nodes_in_neighborhood()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_in_neighborhood.md).

Selections of nodes can also be performed using the following traversal
(`trav_*()`) functions:
[`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md),
[`trav_in()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in.md),
[`trav_both()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both.md),
[`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md),
[`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
[`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md),
or
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md).

## Usage

``` r
trav_both_edge(
  graph,
  conditions = NULL,
  copy_attrs_from = NULL,
  copy_attrs_as = NULL,
  agg = "sum"
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  An option to use filtering conditions for the traversal.

- copy_attrs_from:

  Providing a node attribute name will copy those node attribute values
  to the traversed edges. If the edge attribute already exists, the
  values will be merged to the traversed edges; otherwise, a new edge
  attribute will be created.

- copy_attrs_as:

  If a node attribute name is provided in `copy_attrs_from`, this option
  will allow the copied attribute values to be written under a different
  edge attribute name. If the attribute name provided in `copy_attrs_as`
  does not exist in the graph's edf, the new edge attribute will be
  created with the chosen name.

- agg:

  If a node attribute is provided to `copy_attrs_from`, then an
  aggregation function is required since there may be cases where
  multiple node attribute values will be passed onto the traversed
  edge(s). To pass only a single value, the following aggregation
  functions can be used: `sum`, `min`, `max`, `mean`, or `median`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Set a seed
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a simple graph
graph <-
  create_graph() |>
  add_n_nodes(
    n = 2,
    type = "a",
    label = c("asd", "iekd")) |>
  add_n_nodes(
    n = 3,
    type = "b",
    label = c("idj", "edl", "ohd")) |>
  add_edges_w_string(
    edges = "1->2 1->3 2->4 2->5 3->5",
    rel = c(NA, "A", "B", "C", "D"))

# Create a data frame with node ID values
# representing the graph edges (with `from`
# and `to` columns), and, a set of numeric values
df <-
  data.frame(
    from = c(1, 1, 2, 2, 3),
    to = c(2, 3, 4, 5, 5),
    values = round(rnorm(5, 5), 2))

# Join the data frame to the graph's internal
# edge data frame (edf)
graph <-
  graph |>
  join_edge_attrs(df = df)

# Show the graph's internal edge data frame
graph |> get_edge_df()
#>   id from to  rel values
#> 1  1    1  2 <NA>   6.00
#> 2  2    1  3    A   6.11
#> 3  3    2  4    B   4.72
#> 4  4    2  5    C   6.02
#> 5  5    3  5    D   5.05

# Perform a simple traversal from nodes to
# adjacent edges with no conditions on the
# nodes traversed to
graph |>
  select_nodes_by_id(nodes = 3) |>
  trav_both_edge() |>
  get_selection()
#> [1] 2 5

# Traverse from node `2` to any adjacent
# edges, filtering to those edges that have
# NA values for the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions = is.na(rel)) |>
  get_selection()
#> [1] 1

# Traverse from node `2` to any adjacent
# edges, filtering to those edges that have
# numeric values greater than `6.5` for
# the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions = values > 6.5) |>
  get_selection()
#> [1] 2

# Traverse from node `5` to any adjacent
# edges, filtering to those edges that
# have values equal to `C` for the `rel`
# edge attribute
graph |>
  select_nodes_by_id(nodes = 5) |>
  trav_both_edge(
    conditions = rel == "C") |>
  get_selection()
#> [1] 4

# Traverse from node `2` to any adjacent
# edges, filtering to those edges that
# have values in the set `B` and `C` for
# the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions = rel %in% c("B", "C")) |>
  get_selection()
#> [1] 3 4

# Traverse from node `2` to any adjacent
# edges, and use multiple conditions for the
# traversal
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions =
      rel %in% c("B", "C") &
      values > 4.0) |>
  get_selection()
#> [1] 3 4

# Traverse from node `2` to any adjacent
# edges, and use multiple conditions with
# a single-length vector
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions =
      rel %in% c("B", "C") |
      values > 4.0) |>
  get_selection()
#> [1] 1 3 4

# Traverse from node `2` to any adjacent
# edges, and use a regular expression as
# a filtering condition
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both_edge(
    conditions = grepl("B|C", rel)) |>
  get_selection()
#> [1] 3 4

# Create another simple graph to demonstrate
# copying of node attribute values to traversed
# edges
graph <-
  create_graph() |>
  add_path(n = 4) |>
  select_nodes_by_id(nodes = 2:3) |>
  set_node_attrs_ws(
    node_attr = value,
    value = 5)

# Show the graph's internal edge data frame
graph |>get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>

# Show the graph's internal node data frame
graph |> get_node_df()
#>   id type label value
#> 1  1 <NA>     1    NA
#> 2  2 <NA>     2     5
#> 3  3 <NA>     3     5
#> 4  4 <NA>     4    NA

# Perform a traversal from the nodes to
# the adjacent edges while also applying
# the node attribute `value` to the edges (in
# this case summing the `value` of 5 from
# all contributing nodes adding as an edge
# attribute)
graph <-
  graph |>
  trav_both_edge(
    copy_attrs_from = value,
    agg = "sum")

# Show the graph's internal edge data frame
# after this change
graph |> get_edge_df()
#>   id from to  rel value
#> 1  1    1  2 <NA>     5
#> 2  2    2  3 <NA>    10
#> 3  3    3  4 <NA>     5
```
