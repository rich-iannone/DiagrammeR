# Select edges in a graph

Select edges from a graph object of class `dgr_graph`.

## Usage

``` r
select_edges(
  graph,
  conditions = NULL,
  set_op = "union",
  from = NULL,
  to = NULL,
  edges = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  An option to use filtering conditions for the retrieval of edges.

- set_op:

  The set operation to perform upon consecutive selections of graph
  nodes. This can either be as a `union` (the default), as an
  intersection of selections with `intersect`, or, as a `difference` on
  the previous selection, if it exists.

- from:

  An optional vector of node IDs from which the edge is outgoing for
  filtering the list of edges present in the graph.

- to:

  An optional vector of node IDs to which the edge is incoming for
  filtering the list of edges present in the graph.

- edges:

  An optional vector of edge IDs for filtering the list of edges present
  in the graph.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "basic",
    label = TRUE,
    value = c(3.5, 2.6, 9.4, 2.7))

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = c("a", "z", "a"),
    value = c(6.4, 2.9, 5.0))

# Create a graph with the ndf and edf
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Explicitly select the edge `1`->`4`
graph <-
  graph |>
  select_edges(
    from = 1,
    to = 4)

# Verify that an edge selection has been made
# using the `get_selection()` function
graph |> get_selection()
#> [1] 1

# Select edges based on the relationship label
# being `z`
graph <-
  graph |>
  clear_selection() |>
  select_edges(
    conditions = rel == "z")

# Verify that an edge selection has been made, and
# recall that the `2`->`3` edge uniquely has the
# `z` relationship label
graph |> get_selection()
#> [1] 2

# Select edges based on the edge value attribute
# being greater than 3.0 (first clearing the current
# selection of edges)
graph <-
  graph |>
  clear_selection() |>
  select_edges(
    conditions = value > 3.0)

# Verify that the correct edge selection has been
# made; in this case, edges `1`->`4` and
# `3`->`1` have values for `value` > 3.0
graph |> get_selection()
#> [1] 1 3
```
