# Invert selection of nodes or edges in a graph

Modify the selection of nodes or edges within a graph object such that
all nodes or edges previously not selected will now be selected and vice
versa.

## Usage

``` r
invert_selection(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "standard")

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to")

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Select nodes with ID
# values `1` and `3`
graph <-
  graph |>
  select_nodes(
    nodes = c(1, 3))

# Verify that a node
# selection has been made
graph |> get_selection()
#> [1] 1 3

# Invert the selection
graph <-
  graph |>
  invert_selection()

# Verify that the node
# selection has been changed
graph |> get_selection()
#> [1] 2 4
```
