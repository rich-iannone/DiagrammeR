# Export a graph to various image formats

Export a graph to a variety of image formats such as PNG, PDF, SVG, and
PostScript.

## Usage

``` r
export_graph(
  graph,
  file_name = NULL,
  file_type = NULL,
  title = NULL,
  width = NULL,
  height = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- file_name:

  The name of the exported file (including it's extension).

- file_type:

  The type of file to be exported. Options for graph files are: `png`,
  `pdf`, `svg`, and `ps`.

- title:

  An optional title for the output graph.

- width:

  Output width in pixels or `NULL` for default. Only useful for export
  to image file formats `png`, `pdf`, `svg`, and `ps`.

- height:

  Output height in pixels or `NULL` for default. Only useful for export
  to image file formats `png`, `pdf`, `svg`, and `ps`.

## See also

Other Display and Save:
[`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md),
[`render_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph_from_graph_series.md),
[`save_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/save_graph.md)

## Examples

``` r
# Create a simple graph
graph <-
  create_graph() |>
    add_path(
      n = 5,
      edge_aes = edge_aes(
        arrowhead = c(
          "normal", "vee",
          "tee", "dot"
        ),
        color = c(
        "red", "blue",
        "orange", "purple"
        )
      )
    )

# Create a PDF file for
# the graph (`graph.pdf`)
# graph |>
#   export_graph(
#     file_name = "graph.pdf",
#     title = "Simple Graph"
#   )

# Create a PNG file for
# the graph (`mypng.png`)
# graph |>
#   export_graph(
#     file_name = "mypng.png",
#     file_type = "PNG"
#   )
```
