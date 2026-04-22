# Get the last set of nodes created in a graph

Get the last nodes that were created in a graph object of class
`dgr_graph`. Provides a vector of node ID values. This function should
ideally be used just after creating the nodes.

## Usage

``` r
get_last_nodes_created(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A vector of node ID values.

## Examples

``` r
# Create a graph and add 4 nodes
# in 2 separate function calls
graph <-
  create_graph() |>
  add_n_nodes(
    n = 2,
    type = "a",
    label = c("a_1", "a_2")) |>
  add_n_nodes(
    n = 2,
    type = "b",
    label = c("b_1", "b_2"))

# Get the last nodes created (2 nodes
# from the last function call)
graph |> get_last_nodes_created()
#> [1] 3 4
```
