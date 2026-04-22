# Get community membership by modularity optimization

Through the use of greedy optimization of a modularity score, obtain the
group membership values for each of the nodes in the graph. Note that
this method only works on graphs without multiple edges.

## Usage

``` r
get_cmty_fast_greedy(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

a data frame with group membership assignments for each of the nodes.

## Examples

``` r
# Create a graph with a
# balanced tree
graph <-
  create_graph() |>
  add_balanced_tree(
    k = 2,
    h = 2)

# Get the group membership
# values for all nodes in
# the graph through the greedy
# optimization of modularity
# algorithm
graph |>
  get_cmty_fast_greedy()
#>   id f_g_group
#> 1  1         1
#> 2  2         2
#> 3  3         1
#> 4  4         2
#> 5  5         2
#> 6  6         1
#> 7  7         1

# Add the group membership
# values to the graph as a
# node attribute
graph <-
  graph |>
  join_node_attrs(
    df = get_cmty_fast_greedy(graph))

# Display the graph's
# node data frame
graph |> get_node_df()
#>   id type label f_g_group
#> 1  1 <NA>     1         1
#> 2  2 <NA>     2         2
#> 3  3 <NA>     3         1
#> 4  4 <NA>     4         2
#> 5  5 <NA>     5         2
#> 6  6 <NA>     6         1
#> 7  7 <NA>     7         1
```
