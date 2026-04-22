# Get the maximum graph eccentricity

Get the diameter of a graph, which is the largest eccentricity in the
graph. The graph eccentricity of a node is its shortest path from the
farthest other node in the graph.

## Usage

``` r
get_max_eccentricity(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the maximum eccentricity of the
graph.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine the graph's maximum
# eccentricity
graph |>
  get_max_eccentricity()
#> [1] 4

# Create a full graph and then
# get the maximum eccentricity
# value for that
create_graph() |>
  add_full_graph(n = 10) |>
  get_max_eccentricity()
#> [1] 1
```
