# Add a 2D grid of nodes to the graph

With a graph object of class `dgr_graph`, add a two-dimensional grid to
the graph.

## Usage

``` r
add_grid_2d(
  graph,
  x,
  y,
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

- x:

  The number of nodes in the x direction.

- y:

  The number of nodes in the y direction.

- type:

  An optional string that describes the entity type for the nodes to be
  added.

- label:

  Either a vector object of length `x * y` that provides optional labels
  for the new nodes, or, a logical value where setting to `TRUE`
  ascribes node IDs to the label and `FALSE` yields a blank label.

- rel:

  An optional string for providing a relationship label to all new edges
  created in the grid.

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
# Create a new graph and add
# a 3 x 3 grid
graph <-
  create_graph() |>
  add_grid_2d(
    x = 3, y = 3,
    type = "grid")

# Get node information
# from this graph
graph |>
  get_node_info()
#>   id type label deg indeg outdeg loops
#> 1  1 grid     1   2     0      2     0
#> 2  2 grid     2   3     1      2     0
#> 3  3 grid     3   2     1      1     0
#> 4  4 grid     4   3     1      2     0
#> 5  5 grid     5   4     2      2     0
#> 6  6 grid     6   3     2      1     0
#> 7  7 grid     7   2     1      1     0
#> 8  8 grid     8   3     2      1     0
#> 9  9 grid     9   2     2      0     0

# Attributes can be specified
# in extra arguments and these
# are applied in order; Usually
# these attributes are applied
# to nodes (e.g., `type` is a
# node attribute) but the `rel`
# attribute will apply to the
# edges
graph_w_attrs <-
  create_graph() |>
  add_grid_2d(
    x = 3, y = 2,
    label = c("one", "two",
              "three", "four",
              "five", "six"),
    type = c("a", "a",
             "b", "b",
             "c", "c"),
    rel = "grid",
    node_data = node_data(
      value = c(
        1.2, 8.4, 3.4,
        5.2, 6.1, 2.6)))

# Get the graph's node data frame
graph_w_attrs |> get_node_df()
#>   id type label value
#> 1  1    a   one   1.2
#> 2  2    a   two   8.4
#> 3  3    b three   3.4
#> 4  4    b  four   5.2
#> 5  5    c  five   6.1
#> 6  6    c   six   2.6

# Get the graph's edge data frame
graph_w_attrs |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 grid
#> 2  2    1  4 grid
#> 3  3    2  3 grid
#> 4  4    2  5 grid
#> 5  5    3  6 grid
#> 6  6    4  5 grid
#> 7  7    5  6 grid
```
