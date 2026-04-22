# Is the edge mutual with another edge?

Determines whether an edge definition has a mutual analogue with the
same node pair.

## Usage

``` r
is_edge_mutual(graph, edge)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge:

  A numeric edge ID value.

## Value

A logical value.

## Examples

``` r
# Create a graph that has mutual
# edges across some node pairs
graph <-
  create_graph() |>
  add_path(n = 4) |>
  add_edge(
    from = 4,
    to = 3) |>
  add_edge(
    from = 2,
    to = 1)

# Get the graph's internal
# edge data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    4  3 <NA>
#> 5  5    2  1 <NA>

# Determine if edge `1` has
# a mutual edge
graph |>
  is_edge_mutual(edge = 1)
#> [1] TRUE

# Determine if edge `2` has
# a mutual edge
graph |>
  is_edge_mutual(edge = 2)
#> [1] FALSE
```
