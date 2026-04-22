# Traverse from one or more selected nodes onto adjacent, outward edges

From a graph object of class `dgr_graph` move to outgoing edges from a
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
trav_out_edge(
  graph,
  conditions = NULL,
  copy_attrs_from = NULL,
  copy_attrs_as = NULL
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
    rel = c(NA, "A", "B", "C", "D")) |>
  set_node_attrs(
    node_attr = values,
    values = c(2.3, 4.7, 9.4,
               8.3, 6.3))

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
  join_edge_attrs(
    df = df)

# Show the graph's internal node data frame
graph |> get_node_df()
#>   id type label values
#> 1  1    a   asd    2.3
#> 2  2    a  iekd    4.7
#> 3  3    b   idj    9.4
#> 4  4    b   edl    8.3
#> 5  5    b   ohd    6.3

# Show the graph's internal edge data frame
graph |> get_edge_df()
#>   id from to  rel values
#> 1  1    1  2 <NA>   6.00
#> 2  2    1  3    A   6.11
#> 3  3    2  4    B   4.72
#> 4  4    2  5    C   6.02
#> 5  5    3  5    D   5.05

# Perform a simple traversal from nodes to
# outbound edges with no conditions on the
# nodes traversed to
graph |>
  select_nodes_by_id(nodes = 1) |>
  trav_out_edge() |>
  get_selection()
#> [1] 1 2

# Traverse from node `1` to any outbound
# edges, filtering to those edges that have
# NA values for the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 1) |>
  trav_out_edge(
    conditions = is.na(rel)) |>
  get_selection()
#> [1] 1

# Traverse from node `3` to any outbound
# edges, filtering to those edges that have
# numeric values greater than `5.0` for
# the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 3) |>
  trav_out_edge(
    conditions = values > 5.0) |>
  get_selection()
#> [1] 5

# Traverse from node `1` to any outbound
# edges, filtering to those edges that
# have values equal to `A` for the `rel`
# edge attribute
graph |>
  select_nodes_by_id(nodes = 1) |>
  trav_out_edge(
    conditions = rel == "A") |>
  get_selection()
#> [1] 2

# Traverse from node `2` to any outbound
# edges, filtering to those edges that
# have values in the set `B` and `C` for
# the `rel` edge attribute
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_out_edge(
    conditions = rel %in% c("B", "C")) |>
  get_selection()
#> [1] 3 4

# Traverse from node `2` to any
# outbound edges, and use multiple
# conditions for the traversal
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_out_edge(
    conditions =
      rel %in% c("B", "C") &
      values >= 5.0) |>
  get_selection()
#> [1] 4

# Traverse from node `2` to any
# outbound edges, and use multiple
# conditions
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_out_edge(
    conditions =
      rel %in% c("B", "C") |
      values > 6.0) |>
  get_selection()
#> [1] 3 4

# Traverse from node `2` to any outbound
# edges, and use a regular expression as
# a filtering condition
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_out_edge(
    conditions = grepl("B|C", rel)) |>
  get_selection()
#> [1] 3 4

# Perform a traversal from all nodes to
# their outgoing edges and, while doing
# so, copy the `label` node attribute
# to any of the nodes' incoming edges
graph <-
  graph |>
  select_nodes() |>
  trav_out_edge(
    copy_attrs_from = label)

# Show the graph's internal edge
# data frame after this change
graph |> get_edge_df()
#>   id from to  rel label values
#> 1  1    1  2 <NA>   asd   6.00
#> 2  1    2  3    A   asd   6.11
#> 3  2    3  4    B  iekd   4.72
#> 4  2    4  5    C  iekd   6.02
#> 5  3    5  5    D   idj   5.05
```
