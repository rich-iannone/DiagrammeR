# Add a balanced tree to the graph

With a graph object of class `dgr_graph`, add a balanced tree to the
graph.

## Usage

``` r
add_balanced_tree(
  graph,
  k,
  h,
  type = NULL,
  label = TRUE,
  rel = NULL,
  node_aes = NULL,
  edge_aes = NULL,
  node_data = NULL,
  edge_data = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- k:

  The branching factor for the tree.

- h:

  The height of the tree.

- type:

  An optional string that describes the entity type for the nodes to be
  added.

- label:

  Either a vector object of length `n` that provides optional labels for
  the new nodes, or, a boolean value where setting to `TRUE` ascribes
  node IDs to the label and `FALSE` yields a blank label.

- rel:

  An optional string for providing a relationship label to all new edges
  created in the node tree.

- node_aes:

  An optional list of named vectors comprising node aesthetic
  attributes. The helper function
  [`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md)
  is strongly recommended for use here as it contains arguments for each
  of the accepted node aesthetic attributes (e.g., `shape`, `style`,
  `color`, `fillcolor`).

- edge_aes:

  An optional list of named vectors comprising edge aesthetic
  attributes. The helper function
  [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)
  is strongly recommended for use here as it contains arguments for each
  of the accepted edge aesthetic attributes (e.g., `shape`, `style`,
  `penwidth`, `color`).

- node_data:

  An optional list of named vectors comprising node data attributes. The
  helper function
  [`node_data()`](https://rich-iannone.github.io/DiagrammeR/reference/node_data.md)
  is strongly recommended for use here as it helps bind data
  specifically to the created nodes.

- edge_data:

  An optional list of named vectors comprising edge data attributes. The
  helper function
  [`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md)
  is strongly recommended for use here as it helps bind data
  specifically to the created edges.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a new graph and
# add 2 different types of
# balanced trees of height
# 2 (branching twice) and
# different branching ratios
graph <-
  create_graph() |>
  add_balanced_tree(
    k = 2,
    h = 2,
    type = "binary") |>
  add_balanced_tree(
    k = 3,
    h = 2,
    type = "tertiary")

# Get some node information
# from this graph
graph |>
  get_node_info() |>
  head(5)
#>   id   type label deg indeg outdeg loops
#> 1  1 binary     1   2     0      2     0
#> 2  2 binary     2   3     1      2     0
#> 3  3 binary     3   3     1      2     0
#> 4  4 binary     4   1     1      0     0
#> 5  5 binary     5   1     1      0     0

# Node and edge aesthetic and data
# attributes can be specified in
# the `node_aes`, `edge_aes`,
# `node_data`, and `edge_data`
# arguments
graph_w_attrs <-
  create_graph() |>
  add_balanced_tree(
    k = 2,
    h = 2,
    label = c(
      "one", "two",
      "three", "four",
      "five", "six", "seven"),
    type = c(
      "a", "b", "b", "c",
      "c", "c", "c"),
    rel = "A",
    node_aes = node_aes(
      fillcolor = "steelblue"),
    node_data = node_data(
      value = c(
        1.6, 2.8, 3.4, 8.3,
        3.8, 5.2, 3.2)),
    edge_aes = edge_aes(
      color = "red",
      penwidth = 1.2))

# Get the first three rows of
# the graph's node data frame
graph_w_attrs |>
  get_node_df() |>
  head(3)
#>   id type label fillcolor value
#> 1  1    a   one steelblue   1.6
#> 2  2    b   two steelblue   2.8
#> 3  3    b three steelblue   3.4

# Get the first three rows of
# the graph's edge data frame
graph_w_attrs |>
  get_edge_df() |>
  head(3)
#>   id from to rel penwidth color
#> 1  1    1  2   A      1.2   red
#> 2  2    1  3   A      1.2   red
#> 3  3    2  4   A      1.2   red
```
