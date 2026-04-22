# Get metrics for a graph

Get a data frame with metrics for a graph.

## Usage

``` r
get_graph_info(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame containing metrics pertaining to the graph

## Examples

``` r
if (FALSE) { # \dontrun{
# Import a GML graph file available
# in the DiagrammeR package
karate_club <-
  system.file(
    "extdata", "karate.gml",
    package = "DiagrammeR") |>
  import_graph() |>
  set_graph_name("karate")

# Display a data frame with
# graph information
karate_club |>
  get_graph_info()
} # }
```
