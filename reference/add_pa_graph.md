# Add a preferential attachment graph

To an existing graph object, add a graph built according to the
Barabasi-Albert model, which uses preferential attachment in its
stochastic algorithm.

## Usage

``` r
add_pa_graph(
  graph,
  n,
  m = NULL,
  power = 1,
  out_dist = NULL,
  use_total_degree = FALSE,
  zero_appeal = 1,
  algo = "psumtree",
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

  The number of nodes comprising the preferential attachment graph.

- m:

  The number of edges to add in each time step.

- power:

  The power of the preferential attachment. The default value of `1`
  indicates a linear preferential attachment.

- out_dist:

  A numeric vector that provides the distribution of the number of edges
  to add in each time step.

- use_total_degree:

  A logical value (default is `TRUE`) that governs whether the total
  degree should be used for calculating the citation probability. If
  `FALSE`, the indegree is used.

- zero_appeal:

  A measure of the attractiveness of the nodes with no adjacent edges.

- algo:

  The algorithm to use to generate the graph. The available options are
  `psumtree`, `psumtree-multiple`, and `bag`. With the `psumtree`
  algorithm, a partial prefix-sum tree is used to to create the graph.
  Any values for `power` and `zero_appeal` can be provided and this
  algorithm never generates multiple edges. The `psumtree-multiple`
  algorithm also uses a partial prefix-sum tree but the difference here
  is that multiple edges are allowed. The `bag` algorithm places the
  node IDs into a bag as many times as their in-degree (plus once more).
  The required number of cited nodes are drawn from the bag with
  replacement. Multiple edges may be produced using this method (it is
  not disallowed).

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
# Create an undirected PA
# graph with 100 nodes, adding
# 2 edges at every time step
pa_graph <-
  create_graph(
    directed = FALSE) |>
  add_pa_graph(
    n = 100,
    m = 1)

# Get a count of nodes
pa_graph |> count_nodes()
#> [1] 100

# Get a count of edges
pa_graph |> count_edges()
#> [1] 99
```
