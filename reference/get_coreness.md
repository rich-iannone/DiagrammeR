# Get coreness values for graph nodes

Get the coreness values for all nodes in a graph.

## Usage

``` r
get_coreness(graph, direction = "all")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- direction:

  using `all` (the default), the search will ignore edge direction while
  traversing through the graph. With `out`, measurements of paths will
  be from a node whereas with `in`, measurements of paths will be to a
  node.

## Value

A data frame with coreness values for each of the nodes.

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

# Get coreness values for
# all nodes in the graph
graph |> get_coreness()
#>    id coreness
#> 1   1        3
#> 2   2        3
#> 3   3        2
#> 4   4        3
#> 5   5        3
#> 6   6        3
#> 7   7        3
#> 8   8        2
#> 9   9        2
#> 10 10        0

# Add the coreness values
# to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_coreness(graph))

# Display the graph's node data frame
graph |> get_node_df()
#>    id type label coreness
#> 1   1 <NA>     1        3
#> 2   2 <NA>     2        3
#> 3   3 <NA>     3        2
#> 4   4 <NA>     4        3
#> 5   5 <NA>     5        3
#> 6   6 <NA>     6        3
#> 7   7 <NA>     7        3
#> 8   8 <NA>     8        2
#> 9   9 <NA>     9        2
#> 10 10 <NA>    10        0
```
