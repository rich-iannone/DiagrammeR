# Add a prism of nodes to the graph

With a graph object of class `dgr_graph`, add a node prism to the graph.

## Usage

``` r
add_prism(
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

  The number of nodes describing the shape of the prism. For example,
  the triangular prism has `n` equal to 3 and it is composed of 6 nodes
  and 9 edges. For any n-gonal prism, the graph will be generated with
  2`n` nodes and 3`n` edges.

- type:

  An optional string that describes the entity type for the nodes to be
  added.

- label:

  Either a vector object of length `n` that provides optional labels for
  the new nodes, or, a logical value where setting to `TRUE` ascribes
  node IDs to the label and `FALSE` yields a blank label.

- rel:

  An optional string for providing a relationship label to all new edges
  created in the node prism.

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
# add 2 prisms
graph <-
  create_graph() |>
  add_prism(
    n = 3,
    type = "prism",
    label = "a") |>
  add_prism(
    n = 3,
    type = "prism",
    label = "b")

# Get node information from this graph
graph |> get_node_info()
#>    id  type label deg indeg outdeg loops
#> 1   1 prism     a   3     1      2     0
#> 2   2 prism     a   3     1      2     0
#> 3   3 prism     a   3     1      2     0
#> 4   4 prism     a   3     2      1     0
#> 5   5 prism     a   3     2      1     0
#> 6   6 prism     a   3     2      1     0
#> 7   7 prism     b   3     1      2     0
#> 8   8 prism     b   3     1      2     0
#> 9   9 prism     b   3     1      2     0
#> 10 10 prism     b   3     2      1     0
#> 11 11 prism     b   3     2      1     0
#> 12 12 prism     b   3     2      1     0

# Node and edge aesthetic and data
# attributes can be specified in
# the `node_aes`, `edge_aes`,
# `node_data`, and `edge_data`
# arguments

suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

graph_w_attrs <-
  create_graph() |>
  add_prism(
    n = 3,
    label = c(
      "one", "two",
      "three", "four",
      "five", "six"),
    type = c(
      "a", "a",
      "b", "b",
      "c", "c"),
    rel = "A",
    node_aes = node_aes(
      fillcolor = "steelblue"),
    edge_aes = edge_aes(
      color = "red",
      penwidth = 1.2),
    node_data = node_data(
      value = c(
        1.6, 2.8, 3.4,
        3.2, 5.3, 6.2)),
    edge_data = edge_data(
      value =
        rnorm(
          n = 9,
          mean = 5.0,
          sd = 1.0)))

# Get the graph's node data frame
graph_w_attrs |> get_node_df()
#>   id type label fillcolor value
#> 1  1    a   one steelblue   1.6
#> 2  2    a   two steelblue   2.8
#> 3  3    b three steelblue   3.4
#> 4  4    b  four steelblue   3.2
#> 5  5    c  five steelblue   5.3
#> 6  6    c   six steelblue   6.2

# Get the graph's edge data frame
graph_w_attrs |> get_edge_df()
#>   id from to rel penwidth color    value
#> 1  1    1  2   A      1.2   red 5.996605
#> 2  2    2  3   A      1.2   red 6.107490
#> 3  3    3  1   A      1.2   red 4.721914
#> 4  4    4  5   A      1.2   red 6.019205
#> 5  5    5  6   A      1.2   red 5.045437
#> 6  6    6  4   A      1.2   red 6.575780
#> 7  7    1  4   A      1.2   red 5.218288
#> 8  8    2  5   A      1.2   red 3.953465
#> 9  9    3  6   A      1.2   red 4.711311
```
