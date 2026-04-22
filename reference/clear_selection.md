# Clear an active selection of nodes or edges

Clear the selection of nodes or edges within a graph object.

## Usage

``` r
clear_selection(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph with
# a single path
graph <-
  create_graph() |>
  add_path(n = 5)

# Select nodes with IDs `1`
# and `3`
graph <-
  graph |>
  select_nodes(
    nodes = c(1, 3))

# Verify that a node selection
# has been made
graph |> get_selection()
#> [1] 1 3

# Clear the selection with
# `clear_selection()`
graph <-
  graph |>
  clear_selection()

# Verify that the node
# selection has been cleared
graph |> get_selection()
#> [1] NA
```
