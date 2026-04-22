# Read a graph or graph series from disk

Load a graph or a graph series object from disk.

## Usage

``` r
open_graph(file)
```

## Arguments

- file:

  The filename for the graph or graph series. Optionally, this may
  contain a path to the file.

## Examples

``` r
# Create an undirected GNP
# graph with 100 nodes using
# a probability value of 0.05
gnp_graph <-
  create_graph(
    directed = FALSE) |>
  add_gnp_graph(
    n = 100,
    p = 0.05
  )

# Save the graph to disk; use
# the file name `gnp_graph.dgr`
# save_graph(
#   x = gnp_graph,
#   file = "gnp_graph"
# )

# To read the graph file from
# disk, use `open_graph()`
# gnp_graph_2 <-
#   open_graph(
#     file = "gnp_graph.dgr"
#   )
```
