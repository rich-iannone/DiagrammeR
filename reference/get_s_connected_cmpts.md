# Get nodes within strongly connected components

Determine which nodes in a graph belong to different strongly connected
components.

## Usage

``` r
get_s_connected_cmpts(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame with nodes and their membership in different strongly
connected components.

## Examples

``` r
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a graph with a random
# connection between 2 different
# node cycles
graph <-
  create_graph() |>
  add_cycle(
    n = 3,
    type = "cycle_1") |>
  add_cycle(
    n = 4,
    type = "cycle_2")

graph <-
  graph |>
  add_edge(
    from =
      get_node_ids(
        graph = graph,
        conditions =
          type == "cycle_1") |>
        sample(size = 1),
    to =
      get_node_ids(
        graph = graph,
        conditions =
          type == "cycle_2") |>
        sample(size = 1))

# Get the strongly connected
# components as a data frame of
# nodes and their groupings
graph |> get_s_connected_cmpts()
#>   id sc_component
#> 1  1            1
#> 2  2            1
#> 3  3            1
#> 4  4            2
#> 5  5            2
#> 6  6            2
#> 7  7            2
```
