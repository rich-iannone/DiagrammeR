# Get graph name

Get the name of a graph object of class `dgr_graph`.

## Usage

``` r
get_graph_name(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single-length character vector with the assigned graph name. If a
graph name has not been set, NA is returned.

## Examples

``` r
# Create an empty graph
graph <- create_graph()

# Provide the new graph with a name
graph <-
  set_graph_name(
    graph,
    name = "the_name")

# Get the graph's name
graph |> get_graph_name()
#> [1] "the_name"
```
