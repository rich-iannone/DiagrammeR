# Add a G(n, m) Erdos-Renyi graph

To an existing graph object, add a graph built according to the
Erdos-Renyi G(n, m) model. This uses the same constant probability when
creating the fixed number of edges. Thus for `n` nodes there will be `m`
edges and, if the `loops` argument is set as `TRUE`, then random loop
edges will be part of `m`.

## Usage

``` r
add_gnm_graph(
  graph,
  n,
  m,
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

- m:

  The number of edges in the generated graph.

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
# Create an undirected GNM
# graph with 100 nodes and
# 120 edges
gnm_graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 100,
    m = 120)

# Get a count of nodes
gnm_graph |> count_nodes()
#> [1] 100

# Get a count of edges
gnm_graph |> count_edges()
#> [1] 120
```
