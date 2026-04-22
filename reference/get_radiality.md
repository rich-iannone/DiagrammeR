# Get radiality centrality scores

Get the radiality centrality for all nodes in a graph. These scores
describe the ease to which nodes can reach other nodes.

## Usage

``` r
get_radiality(graph, direction = "all")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- direction:

  Using `all` (the default), the search will ignore edge direction while
  traversing through the graph. With `out`, measurements of paths will
  be from a node whereas with `in`, measurements of paths will be to a
  node.

## Value

A data frame with radiality centrality scores for each of the nodes.

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

# Get the radiality scores for nodes in the graph
graph |>
  get_radiality()
#>    id radiality
#> 1   1    2.3333
#> 2   2    3.0000
#> 3   3    2.6667
#> 4   4    2.8889
#> 5   5    2.5556
#> 6   6    2.4444
#> 7   7    2.6667
#> 8   8    2.7778
#> 9   9    2.1111
#> 10 10    2.3333

# Add the radiality values
# to the graph as a node
# attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_radiality(graph))

# Display the graph's node data frame
graph |>
  get_node_df()
#>    id type label radiality
#> 1   1 <NA>     1    2.3333
#> 2   2 <NA>     2    3.0000
#> 3   3 <NA>     3    2.6667
#> 4   4 <NA>     4    2.8889
#> 5   5 <NA>     5    2.5556
#> 6   6 <NA>     6    2.4444
#> 7   7 <NA>     7    2.6667
#> 8   8 <NA>     8    2.7778
#> 9   9 <NA>     9    2.1111
#> 10 10 <NA>    10    2.3333
```
