# Set graph name

Set a name for a graph object of class `dgr_graph`.

## Usage

``` r
set_graph_name(graph, name)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- name:

  The name to set for the graph.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create an empty graph
graph <- create_graph()

# Provide the new graph with a name
graph <-
  graph |>
  set_graph_name(
    name = "example_name")
```
