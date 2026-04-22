# Get leverage centrality

Get the leverage centrality values for all nodes in the graph. Leverage
centrality is a measure of the relationship between the degree of a
given node and the degree of each of its neighbors, averaged over all
neighbors. A node with negative leverage centrality is influenced by its
neighbors, as the neighbors connect and interact with far more nodes. A
node with positive leverage centrality influences its neighbors since
the neighbors tend to have far fewer connections.

## Usage

``` r
get_leverage_centrality(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame with leverage centrality values for each of the nodes.

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

# Get leverage centrality values
# for all nodes in the graph
graph |>
  get_leverage_centrality()
#>    id leverage_centrality
#> 1   1             -0.1310
#> 2   2             -0.1310
#> 3   3             -0.3810
#> 4   4              0.2357
#> 5   5              0.1270
#> 6   6              0.2079
#> 7   7             -0.0833
#> 8   8             -0.3143
#> 9   9             -0.1000
#> 10 10                 NaN

# Add the leverage centrality
# values to the graph as a
# node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_leverage_centrality(graph))

# Display the graph's node data frame
graph |> get_node_df()
#>    id type label leverage_centrality
#> 1   1 <NA>     1             -0.1310
#> 2   2 <NA>     2             -0.1310
#> 3   3 <NA>     3             -0.3810
#> 4   4 <NA>     4              0.2357
#> 5   5 <NA>     5              0.1270
#> 6   6 <NA>     6              0.2079
#> 7   7 <NA>     7             -0.0833
#> 8   8 <NA>     8             -0.3143
#> 9   9 <NA>     9             -0.1000
#> 10 10 <NA>    10                 NaN
```
