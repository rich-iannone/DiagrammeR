# Get the mean distance

Get the mean distance of a graph, which is the average path length in
the graph. This operates through calculation of the shortest paths
between all pairs of nodes.

## Usage

``` r
get_mean_distance(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the mean distance of the graph.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine the mean distance
graph |>
  get_mean_distance()
#> [1] 2.5

# Create a full graph and then
# get the mean distance value
create_graph() |>
  add_full_graph(n = 10) |>
  get_mean_distance()
#> [1] 1
```
