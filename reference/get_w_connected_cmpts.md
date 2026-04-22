# Get all nodes associated with connected components

Determine which nodes in a graph belong to different weakly connected
components (i.e., distinct sets of nodes with traversable paths to and
from each node in the set).

## Usage

``` r
get_w_connected_cmpts(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame with nodes and their membership in different weakly
connected components.

## Examples

``` r
# Create a graph with 2 cycles
graph <-
  create_graph() |>
  add_cycle(n = 4) |>
  add_cycle(n = 3)

# Check if the graph is connected
graph |>
  is_graph_connected()
#> [1] FALSE

# Get the graph's weakly-connected
# components
graph |> get_w_connected_cmpts()
#>   id wc_component
#> 1  1            1
#> 2  2            1
#> 3  3            1
#> 4  4            1
#> 5  5            2
#> 6  6            2
#> 7  7            2
```
