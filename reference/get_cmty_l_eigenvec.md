# Get community membership by leading eigenvector

Through the calculation of the leading non-negative eigenvector of the
modularity matrix of the graph, obtain the group membership values for
each of the nodes in the graph.

## Usage

``` r
get_cmty_l_eigenvec(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame with group membership assignments for each of the nodes.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get the group membership
# values for all nodes in the
# graph through calculation of
# the leading non-negative
# eigenvector of the modularity
# matrix of the graph
graph |>
  get_cmty_l_eigenvec()
#>    id l_eigenvec_group
#> 1   1                1
#> 2   2                3
#> 3   3                3
#> 4   4                1
#> 5   5                3
#> 6   6                3
#> 7   7                1
#> 8   8                1
#> 9   9                1
#> 10 10                2

# Add the group membership
# values to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_cmty_l_eigenvec(graph))

# Display the graph's node data frame
graph |> get_node_df()
#>    id type label l_eigenvec_group
#> 1   1 <NA>     1                1
#> 2   2 <NA>     2                3
#> 3   3 <NA>     3                3
#> 4   4 <NA>     4                1
#> 5   5 <NA>     5                3
#> 6   6 <NA>     6                3
#> 7   7 <NA>     7                1
#> 8   8 <NA>     8                1
#> 9   9 <NA>     9                1
#> 10 10 <NA>    10                2
```
