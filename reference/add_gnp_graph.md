# Add a G(n, p) Erdos-Renyi graph

To an existing graph object, add a graph built according to the
Erdos-Renyi G(n, p) model, which uses a constant probability when
creating edges.

## Usage

``` r
add_gnp_graph(
  graph,
  n,
  p,
  loops = FALSE,
  type = NULL,
  label = TRUE,
  rel = NULL,
  node_aes = NULL,
  edge_aes = NULL,
  node_data = NULL,
  edge_data = NULL,
  set_seed = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- n:

  The number of nodes comprising the generated graph.

- p:

  The probability of creating an edge between two arbitrary nodes.

- loops:

  A logical value (default is `FALSE`) that governs whether loops are
  allowed to be created.

- type:

  An optional string that describes the entity type for all the nodes to
  be added.

- label:

  A boolean value where setting to `TRUE` ascribes node IDs to the label
  and `FALSE` yields a blank label.

- rel:

  An optional string for providing a relationship label to all edges to
  be added.

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

- set_seed:

  Supplying a value sets a random seed of the `Mersenne-Twister`
  implementation.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create an undirected GNP
# graph with 100 nodes using
# a probability value of 0.05
gnp_graph <-
  create_graph(
    directed = FALSE) |>
  add_gnp_graph(
    n = 100,
    p = 0.05)

# Get a count of nodes
gnp_graph |> count_nodes()
#> [1] 100

# Get a count of edges
gnp_graph |> count_edges()
#> [1] 212
```
