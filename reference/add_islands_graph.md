# Create a random islands graph with edges between the islands

To an existing graph object, add several Erdos-Renyi random graphs (the
islands) using a common set of parameters, connected together by a fixed
number of edges.

## Usage

``` r
add_islands_graph(
  graph,
  n_islands,
  island_size,
  p,
  edges_between,
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

- n_islands:

  The number of islands in the generated graph.

- island_size:

  The size of the islands in the generated graph.

- p:

  The probability of there being edges between the islands.

- edges_between:

  The number of edges between islands.

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
# Create a graph of islands
islands_graph <-
  create_graph() |>
  add_islands_graph(
    n_islands = 4,
    island_size = 10,
    p = 0.5,
    edges_between = 1,
    set_seed = 23)

# Get a count of nodes
islands_graph |> count_nodes()
#> [1] 40

# Get a count of edges
islands_graph |> count_edges()
#> [1] 107
```
