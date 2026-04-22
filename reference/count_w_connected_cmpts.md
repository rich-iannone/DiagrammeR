# Get the number of weakly-connected components

Get the number of weakly-connected components in the graph.

## Usage

``` r
count_w_connected_cmpts(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single integer value representing the number of weakly-connected graph
components.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5) |>
  add_cycle(n = 5)

# Get a count of weakly-connected
# components in the graph
graph |> count_w_connected_cmpts()
#> [1] 2
```
