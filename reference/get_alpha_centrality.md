# Get the alpha centrality for all nodes

Get the alpha centrality values for all nodes in the graph.

## Usage

``` r
get_alpha_centrality(
  graph,
  alpha = 1,
  exo = 1,
  weights_attr = NULL,
  tol = 1e-07
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- alpha:

  the parameter that specifies the relative importance of endogenous
  versus exogenous factors in the determination of centrality.

- exo:

  the exogenous factors, in most cases this is either a constant (which
  applies the same factor to every node), or a vector giving the factor
  for every node.

- weights_attr:

  an optional name of the edge attribute to use in the adjacency matrix.
  If `NULL` then, if it exists, the `weight` edge attribute of the graph
  will be used. Failing that, the standard adjacency matrix will be used
  in calculations.

- tol:

  the tolerance for near-singularities during matrix inversion. The
  default value is set to `1e-7`.

## Value

A data frame with alpha centrality scores for each of the nodes.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 12,
    set_seed = 23)

# Get the alpha centrality scores
# for all nodes
graph |>
  get_alpha_centrality()
#>    id alpha_centrality
#> 1   1                9
#> 2   2                6
#> 3   3                2
#> 4   4                1
#> 5   5                4
#> 6   6                1
#> 7   7                2
#> 8   8                2
#> 9   9                7
#> 10 10                4

# Add the alpha centrality
# scores to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_alpha_centrality(graph))

# Display the graph's node
# data frame
graph |> get_node_df()
#>    id type label alpha_centrality
#> 1   1 <NA>     1                9
#> 2   2 <NA>     2                6
#> 3   3 <NA>     3                2
#> 4   4 <NA>     4                1
#> 5   5 <NA>     5                4
#> 6   6 <NA>     6                1
#> 7   7 <NA>     7                2
#> 8   8 <NA>     8                2
#> 9   9 <NA>     9                7
#> 10 10 <NA>    10                4
```
