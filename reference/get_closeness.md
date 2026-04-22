# Get closeness centrality values

Get the closeness centrality values for all nodes in a graph.

## Usage

``` r
get_closeness(graph, direction = "all")
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

A data frame with closeness values for each of the nodes.

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

# Get closeness values for all nodes
# in the graph
graph |> get_closeness()
#>    id  closeness
#> 1   1 0.05263158
#> 2   2 0.06250000
#> 3   3 0.05555556
#> 4   4 0.06666667
#> 5   5 0.06666667
#> 6   6 0.04166667
#> 7   7 0.04347826
#> 8   8 0.05555556
#> 9   9 0.04166667
#> 10 10 0.04545455

# Add the closeness values to
# the graph as a node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_closeness(graph))

# Display the graph's node data frame
graph |> get_node_df()
#>    id type label  closeness
#> 1   1 <NA>     1 0.05263158
#> 2   2 <NA>     2 0.06250000
#> 3   3 <NA>     3 0.05555556
#> 4   4 <NA>     4 0.06666667
#> 5   5 <NA>     5 0.06666667
#> 6   6 <NA>     6 0.04166667
#> 7   7 <NA>     7 0.04347826
#> 8   8 <NA>     8 0.05555556
#> 9   9 <NA>     9 0.04166667
#> 10 10 <NA>    10 0.04545455
```
