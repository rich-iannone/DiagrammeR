# Add a Watts-Strogatz small-world graph

To an existing graph object, add a graph built according to the
Watts-Strogatz small-world model, which uses a lattice along with a
rewiring probability to randomly modify edge definitions.

## Usage

``` r
add_smallworld_graph(
  graph,
  dimension,
  size,
  neighborhood,
  p,
  loops = FALSE,
  multiple = FALSE,
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

- dimension:

  The dimension of the starting lattice.

- size:

  The size of the lattice across each dimension.

- neighborhood:

  The neighborhood where the lattice nodes are to be connected.

- p:

  The rewiring probability.

- loops:

  A logical value (default is `FALSE`) that governs whether loops are
  allowed to be created.

- multiple:

  A logical value (default is `FALSE`) that governs whether multiple
  edges are allowed to be created.

- type:

  An optional string that describes the entity type for all the nodes to
  be added.

- label:

  A logical value where setting to `TRUE` ascribes node IDs to the label
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
# Create an undirected smallworld
# graph with 100 nodes using
# a probability value of 0.05
smallworld_graph <-
  create_graph(
    directed = FALSE) |>
  add_smallworld_graph(
    dimension = 1,
    size = 50,
    neighborhood = 1,
    p = 0.05,
    set_seed = 23)

# Get a count of nodes
smallworld_graph |> count_nodes()
#> [1] 50

# Get a count of edges
smallworld_graph |> count_edges()
#> [1] 50
```
