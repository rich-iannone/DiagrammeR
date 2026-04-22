# Is the edge a loop edge?

Determines whether an edge definition is a loop edge.

## Usage

``` r
is_edge_loop(graph, edge)
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
# loop edges
graph <-
  create_graph() |>
  add_path(n = 4) |>
  add_edge(
    from = 1,
    to = 1) |>
  add_edge(
    from = 3,
    to = 3)

# Get the graph's internal
# edge data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    1  1 <NA>
#> 5  5    3  3 <NA>

# Determine if edge `4` is
# a loop edge
graph |> is_edge_loop(edge = 4)
#> [1] TRUE

# Determine if edge `2` is
# a loop edge
graph |> is_edge_loop(edge = 2)
#> [1] FALSE
```
