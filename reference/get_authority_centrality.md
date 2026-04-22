# Get the authority scores for all nodes

Get the Kleinberg authority centrality scores for all nodes in the
graph.

## Usage

``` r
get_authority_centrality(graph, weights_attr = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- weights_attr:

  an optional name of the edge attribute to use in the adjacency matrix.
  If `NULL` then, if it exists, the `weight` edge attribute of the graph
  will be used.

## Value

a data frame with authority scores for each of the nodes.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get the authority centrality scores
# for all nodes in the graph
graph |>
  get_authority_centrality()
#>    id authority_centrality
#> 1   1            0.6950912
#> 2   2            0.6176471
#> 3   3            0.0000000
#> 4   4            0.0000000
#> 5   5            0.3674279
#> 6   6            0.3157923
#> 7   7            0.4982552
#> 8   8            1.0000000
#> 9   9            0.1824629
#> 10 10            0.6176471

# Add the authority centrality
# scores to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_authority_centrality(graph))

# Display the graph's node data frame
graph |> get_node_df()
#>    id type label authority_centrality
#> 1   1 <NA>     1            0.6950912
#> 2   2 <NA>     2            0.6176471
#> 3   3 <NA>     3            0.0000000
#> 4   4 <NA>     4            0.0000000
#> 5   5 <NA>     5            0.3674279
#> 6   6 <NA>     6            0.3157923
#> 7   7 <NA>     7            0.4982552
#> 8   8 <NA>     8            1.0000000
#> 9   9 <NA>     9            0.1824629
#> 10 10 <NA>    10            0.6176471
```
