# Get the minimum graph eccentricity

Get the radius of a graph, which is the smallest eccentricity in the
graph. The graph eccentricity of a node is its shortest path from the
farthest other node in the graph.

## Usage

``` r
get_min_eccentricity(graph, direction = "all")
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

A single numeric value representing the minimum eccentricity of the
graph.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine the graph's minimum
# eccentricity
graph |>
  get_min_eccentricity()
#> [1] 2

# Create a full graph and then
# get the minimum eccentricity
# value for that
create_graph() |>
  add_full_graph(n = 10) |>
  get_min_eccentricity()
#> [1] 1
```
