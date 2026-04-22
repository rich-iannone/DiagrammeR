# Get the graph girth

Get the girth of a graph, which is the length of the shortest circle in
the graph. Loop edges and multiple edges are not considered. If the
graph contains no cycles then zero is returned.

## Usage

``` r
get_girth(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the length of the shortest circle in
the graph.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine the graph's girth
graph |> get_girth()
#> [1] 5

# Create a full graph and then
# get the girth for that
create_graph() |>
  add_full_graph(n = 10) |>
  get_girth()
#> [1] 3
```
