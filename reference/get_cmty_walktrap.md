# Get community membership using the Walktrap method

With the Walktrap community finding algorithm, obtain the group
membership values for each of the nodes in the graph.

## Usage

``` r
get_cmty_walktrap(graph, steps = 4)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- steps:

  the number of steps to take for each of the random walks.

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
# graph through the Walktrap
# community finding algorithm
graph |>
  get_cmty_walktrap()
#>    id walktrap_group
#> 1   1              3
#> 2   2              2
#> 3   3              2
#> 4   4              1
#> 5   5              2
#> 6   6              2
#> 7   7              3
#> 8   8              1
#> 9   9              1
#> 10 10              4

# Add the group membership
# values to the graph as a
# node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_cmty_walktrap(graph))

# Display the graph's
# node data frame
graph |> get_node_df()
#>    id type label walktrap_group
#> 1   1 <NA>     1              3
#> 2   2 <NA>     2              2
#> 3   3 <NA>     3              2
#> 4   4 <NA>     4              1
#> 5   5 <NA>     5              2
#> 6   6 <NA>     6              2
#> 7   7 <NA>     7              3
#> 8   8 <NA>     8              1
#> 9   9 <NA>     9              1
#> 10 10 <NA>    10              4
```
