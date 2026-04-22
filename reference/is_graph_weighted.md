# Is the graph a weighted graph?

Provides a logical value on whether the graph is weighted. A graph is
considered to be weighted when it contains edges that all have a edge
`weight` attribute with numerical values assigned for all edges.

## Usage

``` r
is_graph_weighted(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create a graph where the edges have
# a `weight` attribute
graph <-
  create_graph() |>
  add_cycle(n = 5) |>
  select_edges() |>
  set_edge_attrs_ws(
    edge_attr = weight,
    value = c(3, 5, 2, 9, 6)) |>
  clear_selection()

# Determine whether the graph
# is a weighted graph
graph |> is_graph_weighted()
#> [1] TRUE

# Create graph where the edges do
# not have a `weight` attribute
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Determine whether this graph
# is weighted
graph |> is_graph_weighted()
#> [1] FALSE
```
