# Get the number of strongly-connected components

Get the number of strongly-connected components in the graph.

## Usage

``` r
count_s_connected_cmpts(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single integer value representing the number of strongly-connected
graph components.

## Examples

``` r
# Create a graph and add
# several graph islands
graph <-
  create_graph() |>
  add_islands_graph(
    n_islands = 4,
    island_size = 10,
    p = 1/5,
    edges_between = 1,
    set_seed = 23)

# Get a count of strongly-connected
# components in the graph
graph |> count_s_connected_cmpts()
#> [1] 2
```
