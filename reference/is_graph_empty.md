# Is the graph empty?

Provides a logical value on whether the graph is empty (i.e., contains
no nodes).

## Usage

``` r
is_graph_empty(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A logical value.

## Examples

``` r
# Create an empty graph
graph <- create_graph()

# Determine whether the graph is empty
graph |> is_graph_empty()
#> [1] TRUE

# Create a non-empty graph
graph <-
  create_graph() |>
  add_n_nodes(n = 3)

# Determine whether this graph is empty
graph |> is_graph_empty()
#> [1] FALSE
```
