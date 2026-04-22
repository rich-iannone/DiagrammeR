# Save a graph or graph series to disk

Save a graph or a graph series object to disk.

## Usage

``` r
save_graph(x, file)
```

## Arguments

- x:

  A graph object of class `dgr_graph` or a graph series object of type
  `dgr_graph_1D`.

- file:

  A file name for the graph or graph series. Provide a character string
  and the `.dgr` extension will be applied to it.

## See also

Other Display and Save:
[`export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.md),
[`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md),
[`render_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph_from_graph_series.md)

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
    p = 0.05)

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
# )
```
