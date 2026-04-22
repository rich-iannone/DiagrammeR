# Is the graph a simple graph?

Determine whether the graph is a simple graph. A simple graph is one
that does not contain any loops nor any multiple edges.

## Usage

``` r
is_graph_simple(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create a graph with 2 cycles
graph <-
  create_graph() |>
  add_cycle(n = 4) |>
  add_cycle(n = 3)

# Check if the graph is simple
graph |> is_graph_simple()
#> [1] TRUE
```
