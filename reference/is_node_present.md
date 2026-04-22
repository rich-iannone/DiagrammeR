# Determine whether a specified node is present

From a graph object of class `dgr_graph`, determine whether a specified
node is present.

## Usage

``` r
is_node_present(graph, node)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  Either a node ID value or a node label to test for presence in the
  graph.

## Value

A logical value.

## Examples

``` r
# Create a simple graph with
# a path of four nodes
graph <-
  create_graph() |>
  add_path(
    n = 4,
    type = "path",
    label = c(
      "one", "two",
      "three", "four"))

# Determine if there is a node
# with ID `1` in the graph
graph |>
  is_node_present(node = 1)
#> [1] TRUE

# Determine if there is a node
# with ID `5` in the graph
graph |>
  is_node_present(node = 5)
#> [1] FALSE

# Determine if there is a node
# with label `two` in the graph
graph |>
  is_node_present(node = "two")
#> [1] TRUE
```
