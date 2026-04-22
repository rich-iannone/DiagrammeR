# Get community membership by Louvain optimization

Through the use of multi-level optimization of a modularity score,
obtain the group membership values for each of the nodes in the graph.

## Usage

``` r
get_cmty_louvain(graph)
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

# Get the group membership values
# for all nodes in the graph
# through the multi-level
# optimization of modularity
# algorithm
graph |>
  get_cmty_louvain()
#>    id louvain_group
#> 1   1             1
#> 2   2             2
#> 3   3             2
#> 4   4             3
#> 5   5             2
#> 6   6             2
#> 7   7             1
#> 8   8             3
#> 9   9             3
#> 10 10             4

# Add the group membership
# values to the graph as a
# node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_cmty_louvain(graph))

# Display the graph's
# node data frame
graph |> get_node_df()
#>    id type label louvain_group
#> 1   1 <NA>     1             1
#> 2   2 <NA>     2             2
#> 3   3 <NA>     3             2
#> 4   4 <NA>     4             3
#> 5   5 <NA>     5             2
#> 6   6 <NA>     6             2
#> 7   7 <NA>     7             1
#> 8   8 <NA>     8             3
#> 9   9 <NA>     9             3
#> 10 10 <NA>    10             4
```
