# Traverse to any reverse edges

From an active selection of edges in a graph object of class
`dgr_graph`, traverse to any available reverse edges between the nodes
common to the selected edges. For instance, if an active selection has
the edge `1->2` but there is also an (not selected) edge `2->1`, then
this function can either switch to the selection of `2->1`, or,
incorporate both those edges into the active selection of edges.

This traversal function makes use of an active selection of edges. After
the traversal, depending on the traversal conditions, there will either
be a selection of edges or no selection at all.

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
or `trav_reverse_edge()`.

## Usage

``` r
trav_reverse_edge(graph, add_to_selection = FALSE)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- add_to_selection:

  An option to either add the reverse edges to the active selection of
  edges (`TRUE`) or switch the active selection entirely to those
  reverse edges (`FALSE`, the default case).

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "basic",
    label = TRUE)

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 4, 2, 3, 3),
    to =   c(4, 1, 3, 2, 1))

# Create a graph with the
# ndf and edf
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Explicitly select the edges
# `1`->`4` and `2`->`3`
graph <-
  graph |>
  select_edges(
    from = 1,
      to = 4) |>
  select_edges(
    from = 2,
      to = 3)

# Get the inital edge selection
graph |> get_selection()
#> [1] 1 3

# Traverse to the reverse edges
# (edges `2`: `4`->`1` and
# `4`:`3`->`2`)
graph <-
  graph |>
  trav_reverse_edge()

# Get the current selection of edges
graph |> get_selection()
#> [1] 2 4
```
