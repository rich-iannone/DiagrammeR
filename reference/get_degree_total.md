# Get total degree values for all nodes

Get the total degree values for all nodes in a graph.

## Usage

``` r
get_degree_total(graph, normalized = FALSE)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- normalized:

  set as `FALSE` (the default), the total degree will be provided for
  each of the nodes (as a count of edges to and from each node). When
  set as `TRUE`, then the result for each node will be divided by the
  total number of nodes in the graph minus 1.

## Value

A data frame with total degree values for each of the nodes.

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

# Get the total degree values
# for all nodes in the graph
graph |>
  get_degree_total()
#>    id total_degree
#> 1   1            3
#> 2   2            3
#> 3   3            2
#> 4   4            5
#> 5   5            4
#> 6   6            5
#> 7   7            3
#> 8   8            2
#> 9   9            3
#> 10 10            0

# Add the total degree values
# to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_degree_total(graph))

# Display the graph's
# node data frame
graph |> get_node_df()
#>    id type label total_degree
#> 1   1 <NA>     1            3
#> 2   2 <NA>     2            3
#> 3   3 <NA>     3            2
#> 4   4 <NA>     4            5
#> 5   5 <NA>     5            4
#> 6   6 <NA>     6            5
#> 7   7 <NA>     7            3
#> 8   8 <NA>     8            2
#> 9   9 <NA>     9            3
#> 10 10 <NA>    10            0
```
