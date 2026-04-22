# Traverse from one or more selected nodes onto neighboring nodes

From a graph object of class `dgr_graph` move from one or more nodes
present in a selection to other nodes that are connected by edges,
replacing the current nodes in the selection with those nodes traversed
to. An optional filter by node attribute can limit the set of nodes
traversed to.

This traversal function makes use of an active selection of nodes. After
the traversal, depending on the traversal conditions, there will either
be a selection of nodes or no selection at all.

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
`trav_both()`,
[`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md),
[`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
[`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md),
or
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md).

## Usage

``` r
trav_both(
  graph,
  conditions = NULL,
  copy_attrs_from = NULL,
  copy_attrs_as = NULL,
  agg = "sum",
  add_to_selection = FALSE
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  An option to use filtering conditions for the traversal.

- copy_attrs_from:

  Providing a node attribute name will copy those node attribute values
  to the traversed nodes. Any values extant on the nodes traversed to
  will be replaced.

- copy_attrs_as:

  If a node attribute name is provided in `copy_attrs_from`, this option
  will allow the copied attribute values to be written under a different
  attribute name. If the attribute name provided in `copy_attrs_as` does
  not exist in the graph's ndf, the new node attribute will be created
  with the chosen name.

- agg:

  If a node attribute is provided to `copy_attrs_from`, then an
  aggregation function is required since there may be cases where
  multiple edge attribute values will be passed onto the traversed
  node(s). To pass only a single value, the following aggregation
  functions can be used: `sum`, `min`, `max`, `mean`, or `median`.

- add_to_selection:

  An option to either add the traversed to nodes to the active selection
  of nodes (`TRUE`) or switch the active selection entirely to those
  traversed to nodes (`FALSE`, the default case).

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
df_edges <-
  data.frame(
    from = c(1, 1, 2, 2, 3),
    to = c(2, 3, 4, 5, 5),
    values = round(rnorm(5, 5), 2))

# Create a data frame with node ID values
# representing the graph nodes (with the `id`
# columns), and, a set of numeric values
df_nodes <-
  data.frame(
    id = 1:5,
    values = round(rnorm(5, 7), 2))

# Join the data frame to the graph's internal
# edge data frame (edf)
graph <-
  graph |>
  join_edge_attrs(df = df_edges) |>
  join_node_attrs(df = df_nodes)

# Show the graph's internal node data frame
graph |> get_node_df()
#>   id type label values
#> 1  1    a   asd   8.58
#> 2  2    a  iekd   7.22
#> 3  3    b   idj   5.95
#> 4  4    b   edl   6.71
#> 5  5    b   ohd   7.48

# Show the graph's internal edge data frame
graph |> get_edge_df()
#>   id from to  rel values
#> 1  1    1  2 <NA>   6.00
#> 2  2    1  3    A   6.11
#> 3  3    2  4    B   4.72
#> 4  4    2  5    C   6.02
#> 5  5    3  5    D   5.05

# Perform a simple traversal from node `3`
# to adjacent nodes with no conditions on
# the nodes traversed to
graph |>
  select_nodes_by_id(nodes = 3) |>
  trav_both() |>
  get_selection()
#> [1] 1 5

# Traverse from node `2` to any adjacent
# nodes, filtering to those nodes that have
# numeric values less than `8.0` for
# the `values` node attribute
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both(
    conditions = values < 8.0) |>
  get_selection()
#> [1] 4 5

# Traverse from node `5` to any adjacent
# nodes, filtering to those nodes that
# have a `type` attribute of `b`
graph |>
  select_nodes_by_id(nodes = 5) |>
  trav_both(
    conditions = type == "b") |>
  get_selection()
#> [1] 3

# Traverse from node `2` to any adjacent
# nodes, and use multiple conditions for the
# traversal
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both(
    conditions =
      type == "a" &
      values > 8.0) |>
  get_selection()
#> [1] 1

# Traverse from node `2` to any adjacent
# nodes, and use multiple conditions with
# a single-length vector
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both(
    conditions =
      type == "a" | values > 8.0) |>
  get_selection()
#> [1] 1

# Traverse from node `2` to any adjacent
# nodes, and use a regular expression as
# a filtering condition
graph |>
  select_nodes_by_id(nodes = 2) |>
  trav_both(
    conditions = grepl("..d", label)) |>
  get_selection()
#> [1] 1 5

# Create another simple graph to demonstrate
# copying of node attribute values to traversed
# nodes
graph <-
  create_graph() |>
  add_path(n = 5) |>
  select_nodes_by_id(nodes = c(2, 4)) |>
  set_node_attrs_ws(
    node_attr = value,
    value = 5)

# Show the graph's internal node data frame
graph |> get_node_df()
#>   id type label value
#> 1  1 <NA>     1    NA
#> 2  2 <NA>     2     5
#> 3  3 <NA>     3    NA
#> 4  4 <NA>     4     5
#> 5  5 <NA>     5    NA

# Show the graph's internal edge data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    4  5 <NA>

# Perform a traversal from the inner nodes
# (`2` and `4`) to their adjacent nodes (`1`,
# `3`, and `5`) while also applying the node
# attribute `value` to target nodes; node `3`
# will obtain a `value` of 10 since a traversal
# to `3` will occur from `2` and `4` (and
# multiple values passed will be summed)
graph <-
  graph |>
  trav_both(
    copy_attrs_from = value,
    agg = "sum")

# Show the graph's internal node data frame
# after this change
graph |> get_node_df()
#>   id type label value
#> 1  1 <NA>     1     5
#> 2  3 <NA>     3    10
#> 3  5 <NA>     5     5
#> 4  2 <NA>     2     5
#> 5  4 <NA>     4     5
```
