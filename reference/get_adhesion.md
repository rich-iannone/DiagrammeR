# Get graph adhesion

Get the adhesion of a graph, which is the minimum number of edges needed
to remove to obtain a graph which is not strongly connected. This is the
same as the edge connectivity of the graph.

## Usage

``` r
get_adhesion(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the minimum number of edges to
remove.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine the graph's adhesion
graph |> get_adhesion()
#> [1] 1

# Create a full graph and then
# get the adhesion for that
create_graph() |>
  add_full_graph(n = 8) |>
  get_adhesion()
#> [1] 7
```
