# Render the graph in various formats

Using a `dgr_graph` object, render the graph in the RStudio Viewer.

## Usage

``` r
render_graph(
  graph,
  layout = NULL,
  output = NULL,
  as_svg = FALSE,
  title = NULL,
  width = NULL,
  height = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- layout:

  A string specifying a layout type to use for node placement in this
  rendering. Possible layouts include: `nicely`, `circle`, `tree`, `kk`,
  and `fr`.

- output:

  A string specifying the output type; `graph` (the default) renders the
  graph using the
  [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  function and `visNetwork` renders the graph using the
  [`visnetwork()`](https://rich-iannone.github.io/DiagrammeR/reference/visnetwork.md)
  function.

- as_svg:

  An option to render the graph as an SVG document.

- title:

  An optional title for a graph when using `output = "graph"`.

- width:

  An optional parameter for specifying the width of the resulting
  graphic in pixels.

- height:

  An optional parameter for specifying the height of the resulting
  graphic in pixels.

## See also

Other Display and Save:
[`export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.md),
[`render_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph_from_graph_series.md),
[`save_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/save_graph.md)

## Examples

``` r
if (interactive()) {

  # Render a graph that's a
  # balanced tree
  create_graph() |>
    add_balanced_tree(
      k = 2, h = 3
    ) |>
    render_graph()

  # Use the `tree` layout for
  # better node placement in this
  # hierarchical graph
  create_graph() |>
    add_balanced_tree(
      k = 2, h = 3
    ) |>
    render_graph(layout = "tree")

  # Plot the same tree graph but
  # don't show the node ID values
  create_graph() |>
    add_balanced_tree(
      k = 2, h = 3
    ) |>
    set_node_attr_to_display() |>
    render_graph(layout = "tree")

  # Create a circle graph
  create_graph() |>
    add_gnm_graph(
      n = 55,
      m = 75,
      set_seed = 23
    ) |>
    render_graph(
      layout = "circle"
    )

  # Render the graph using the
  # `visNetwork` output option
  create_graph() |>
    add_balanced_tree(
      k = 2, h = 3
    ) |>
    render_graph(
      output = "visNetwork"
    )
}
```
