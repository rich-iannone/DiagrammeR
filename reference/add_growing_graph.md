# Create a random growing graph with m edges added per step

To an existing graph object, add a graph built by adding `m` new edges
at each time step (where a node is added).

## Usage

``` r
add_growing_graph(
  graph,
  n,
  m = 1,
  citation = FALSE,
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

  The number of edges added per time step.

- citation:

  A logical value (default is `FALSE`) that governs whether a citation
  graph is to be created. This is where new edges specifically originate
  from the newly added node in the most recent time step.

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

## Examples

``` r
# Create a random, growing
# citation graph with 100
# nodes, adding an edge after
# each node addition
growing_graph <-
  create_graph() |>
  add_growing_graph(
    n = 100,
    m = 1,
    citation = TRUE,
    set_seed = 23)

# Get a count of nodes
growing_graph |> count_nodes()
#> [1] 100

# Get a count of edges
growing_graph |> count_edges()
#> [1] 99
```
