# Add a star of nodes to the graph

With a graph object of class `dgr_graph`, add a node star to the graph.

## Usage

``` r
add_star(
  graph,
  n,
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

- n:

  The number of nodes comprising the star. The first node will be the
  center of the star.

- type:

  An optional string that describes the entity type for the nodes to be
  added.

- label:

  Either a vector object of length `n` that provides optional labels for
  the new nodes, or, a logical value where setting to `TRUE` ascribes
  node IDs to the label and `FALSE` yields a blank label.

- rel:

  An optional string for providing a relationship label to all new edges
  created in the node star.

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
# Create a new graph and add 2
# stars of varying numbers of nodes
graph <-
  create_graph() |>
  add_star(
    n = 4,
    type = "four_star") |>
  add_star(
    n = 5,
    type = "five_star")

# Get node information from this graph
graph |> get_node_info()
#>   id      type label deg indeg outdeg loops
#> 1  1 four_star     1   3     0      3     0
#> 2  2 four_star     2   1     1      0     0
#> 3  3 four_star     3   1     1      0     0
#> 4  4 four_star     4   1     1      0     0
#> 5  5 five_star     5   4     0      4     0
#> 6  6 five_star     6   1     1      0     0
#> 7  7 five_star     7   1     1      0     0
#> 8  8 five_star     8   1     1      0     0
#> 9  9 five_star     9   1     1      0     0

# Node and edge aesthetic and data
# attributes can be specified in
# the `node_aes`, `edge_aes`,
# `node_data`, and `edge_data`
# arguments

suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

graph_w_attrs <-
  create_graph() |>
  add_star(
    n = 4,
    label = c(
      "one", "two",
      "three", "four"),
    type = c(
      "a", "a", "b", "b"),
    rel = "A",
    node_aes = node_aes(
      fillcolor = "steelblue"),
    edge_aes = edge_aes(
      color = "red",
      penwidth = 1.2),
    node_data = node_data(
      value = c(
        1.6, 2.8, 3.4, 8.3)),
    edge_data = edge_data(
      value =
        rnorm(
          n = 3,
          mean = 5.0,
          sd = 1.0)))

# Get the graph's node data frame
graph_w_attrs |> get_node_df()
#>   id type label fillcolor value
#> 1  1    a   one steelblue   1.6
#> 2  2    a   two steelblue   2.8
#> 3  3    b three steelblue   3.4
#> 4  4    b  four steelblue   8.3

# Get the graph's edge data frame
graph_w_attrs |> get_edge_df()
#>   id from to rel penwidth color    value
#> 1  1    1  2   A      1.2   red 5.996605
#> 2  2    1  3   A      1.2   red 6.107490
#> 3  3    1  4   A      1.2   red 4.721914
```
