# Render a graph available in a series

Using a graph series object of type `dgr_graph_1D`, either render graph
in the Viewer or output in various formats.

## Usage

``` r
render_graph_from_graph_series(
  graph_series,
  graph_no,
  output = "graph",
  width = NULL,
  height = NULL
)
```

## Arguments

- graph_series:

  A graph series object of type `dgr_graph_1D`.

- graph_no:

  The index of the graph in the graph series.

- output:

  A string specifying the output type; `graph` (the default) renders the
  graph using the
  [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  function and `visNetwork` renders the graph using the
  [`visnetwork()`](https://rich-iannone.github.io/DiagrammeR/reference/visnetwork.md)
  function.

- width:

  An optional parameter for specifying the width of the resulting
  graphic in pixels.

- height:

  An optional parameter for specifying the height of the resulting
  graphic in pixels.

## See also

Other Display and Save:
[`export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.md),
[`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md),
[`save_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/save_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create three graphs
graph_1 <-
  create_graph() |>
  add_path(n = 4)

graph_2 <-
  create_graph() |>
  add_cycle(n = 5)

graph_3 <-
  create_graph() |>
  add_star(n = 6)

# Create an empty graph series
# and add the graphs
series <-
  create_graph_series() |>
  add_graph_to_graph_series(
    graph = graph_1) |>
  add_graph_to_graph_series(
    graph = graph_2) |>
  add_graph_to_graph_series(
    graph = graph_3)

# View the second graph in
# the series in the Viewer
render_graph_from_graph_series(
  graph_series = series,
  graph_no = 2)
} # }
```
