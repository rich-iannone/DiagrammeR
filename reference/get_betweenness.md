# Get betweenness centrality scores

Get the betweenness centrality scores for all nodes in a graph.

## Usage

``` r
get_betweenness(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

a data frame with betweenness scores for each of the nodes.

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

# Get the betweenness scores
# for nodes in the graph
graph |> get_betweenness()
#>    id betweenness
#> 1   1           0
#> 2   2           7
#> 3   3           0
#> 4   4           0
#> 5   5           5
#> 6   6           0
#> 7   7           0
#> 8   8           1
#> 9   9           0
#> 10 10           0

# Add the betweenness
# values to the graph
# as a node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_betweenness(graph))

# Display the graph's node
# data frame
graph |> get_node_df()
#>    id type label betweenness
#> 1   1 <NA>     1           0
#> 2   2 <NA>     2           7
#> 3   3 <NA>     3           0
#> 4   4 <NA>     4           0
#> 5   5 <NA>     5           5
#> 6   6 <NA>     6           0
#> 7   7 <NA>     7           0
#> 8   8 <NA>     8           1
#> 9   9 <NA>     9           0
#> 10 10 <NA>    10           0
```
