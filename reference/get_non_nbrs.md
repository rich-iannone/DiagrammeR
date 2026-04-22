# Get non-neighbors of a node in a graph

Get the set of all nodes not neighboring a single graph node.

## Usage

``` r
get_non_nbrs(graph, node)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  A single-length vector containing a node ID value.

## Value

A vector of node ID values.

## Examples

``` r
# Create a simple, directed graph with 5
# nodes and 4 edges
graph <-
  create_graph() |>
  add_path(n = 5)

# Find all non-neighbors of node `2`
graph |> get_non_nbrs(node = 2)
#> [1] 4 5
```
