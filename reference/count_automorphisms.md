# Get the number of automorphisms

Get the number of automorphisms the graph contains. An automorphism of a
graph is a form of symmetry in which the graph is mapped onto itself
while preserving edge-node connectivity.

## Usage

``` r
count_automorphisms(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the number of automorphisms the
graph contains.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Get a count of automorphisms
graph |>
  count_automorphisms()
#> [1] 10

# Create a full graph and then
# count the automorphisms
create_graph() |>
  add_full_graph(n = 10) |>
  count_automorphisms()
#> [1] 3628800
```
