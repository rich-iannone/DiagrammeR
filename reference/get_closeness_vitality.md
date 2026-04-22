# Get closeness vitality

Get the closeness vitality values for all nodes in the graph.

## Usage

``` r
get_closeness_vitality(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame with closeness vitality values for each of the nodes.

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

# Get closeness vitality values
# for all nodes in the graph
graph |> get_closeness_vitality()
#>    id closeness_vitality
#> 1   1                 32
#> 2   2                118
#> 3   3                 36
#> 4   4                 60
#> 5   5                  0
#> 6   6                 48
#> 7   7                 46
#> 8   8                 30
#> 9   9                 48
#> 10 10                 44

# Add the closeness vitality
# values to the graph as a
# node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_closeness_vitality(graph))

# Display the graph's
# node data frame
graph |> get_node_df()
#>    id type label closeness_vitality
#> 1   1 <NA>     1                 32
#> 2   2 <NA>     2                118
#> 3   3 <NA>     3                 36
#> 4   4 <NA>     4                 60
#> 5   5 <NA>     5                  0
#> 6   6 <NA>     6                 48
#> 7   7 <NA>     7                 46
#> 8   8 <NA>     8                 30
#> 9   9 <NA>     9                 48
#> 10 10 <NA>    10                 44
```
