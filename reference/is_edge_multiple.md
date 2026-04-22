# Is the edge a multiple edge?

Determines whether an edge definition has multiple edge IDs associated
with the same node pair.

## Usage

``` r
is_edge_multiple(graph, edge)
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
# Create a graph that has multiple
# edges across some node pairs
graph <-
  create_graph() |>
  add_path(n = 4) |>
  add_edge(
    from = 1,
    to = 2) |>
  add_edge(
    from = 3,
    to = 4)

# Get the graph's internal
# edge data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    1  2 <NA>
#> 5  5    3  4 <NA>

# Determine if edge `1` is
# a multiple edge
graph |>
  is_edge_multiple(edge = 1)
#> [1] TRUE

# Determine if edge `2` is
# a multiple edge
graph |>
  is_edge_multiple(edge = 2)
#> [1] FALSE
```
