# Get nodes that form the graph periphery

Get those nodes that are part of the graph periphery (i.e., have the
maximum eccentricity in the graph).

## Usage

``` r
get_periphery(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A vector of node IDs.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function and
# get the nodes in the graph periphery
create_graph() |>
  add_gnm_graph(
    n = 28,
    m = 35,
    set_seed = 23) |>
  get_periphery()
#> [1]  6  9 25 28
```
