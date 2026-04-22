# Get the last set of edges created in a graph

Get the last edges that were created in a graph object of class
`dgr_graph`. This function should ideally be used just after creating
the edges.

## Usage

``` r
get_last_edges_created(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A vector of edge ID values.

## Examples

``` r
# Create a graph and add a cycle and then
# a tree in 2 separate function calls
graph <-
  create_graph() |>
  add_cycle(
    n = 3,
    rel = "a") |>
  add_balanced_tree(
    k = 2, h = 2,
    rel = "b")

# Get the last edges created (all edges
# from the tree)
graph |> get_last_edges_created()
#> [1] 4 5 6 7 8 9
```
