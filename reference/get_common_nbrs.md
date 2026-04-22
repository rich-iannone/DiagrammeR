# Get all common neighbors between two or more nodes

With two or more nodes, get the set of common neighboring nodes.

## Usage

``` r
get_common_nbrs(graph, nodes)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  a vector of node ID values of length at least 2.

## Value

a vector of node ID values.

## Examples

``` r
# Create a directed graph with 5 nodes
graph <-
  create_graph() |>
  add_path(n = 5)

# Find all common neighbor nodes
# for nodes `1` and `2` (there are no
# common neighbors amongst them)
graph |>
  get_common_nbrs(
    nodes = c(1, 2))
#> [1] NA

# Find all common neighbor nodes for
# nodes `1` and `3`
graph |>
  get_common_nbrs(
    nodes = c(1, 3))
#> [1] 2
```
