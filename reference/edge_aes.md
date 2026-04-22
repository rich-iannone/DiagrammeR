# Insert edge aesthetic attributes during edge creation

This helper function should be invoked to provide values for the
namesake `edge_aes` argument, which is present in any function where
edges are created.

## Usage

``` r
edge_aes(
  style = NULL,
  penwidth = NULL,
  color = NULL,
  arrowsize = NULL,
  arrowhead = NULL,
  arrowtail = NULL,
  fontname = NULL,
  fontsize = NULL,
  fontcolor = NULL,
  len = NULL,
  tooltip = NULL,
  URL = NULL,
  label = NULL,
  labelfontname = NULL,
  labelfontsize = NULL,
  labelfontcolor = NULL,
  labeltooltip = NULL,
  labelURL = NULL,
  edgetooltip = NULL,
  edgeURL = NULL,
  dir = NULL,
  headtooltip = NULL,
  headURL = NULL,
  headclip = NULL,
  headlabel = NULL,
  headport = NULL,
  tailtooltip = NULL,
  tailURL = NULL,
  tailclip = NULL,
  taillabel = NULL,
  tailport = NULL,
  decorate = NULL
)
```

## Arguments

- style:

  The edge line style. The `style` types that can be used are `solid`,
  `bold`, `dashed`, `dotted`, `tapered`, and `invisible`.

- penwidth:

  The thickness of the stroke line for the edge itself.

- color:

  The color of the edge. Can be an X11 color or a hexadecimal color
  code.

- arrowsize:

  A scaling factor for arrowheads. The default value is `1.0` and the
  minimum is `0`.

- arrowhead:

  The type of arrowhead to use. The `style` attribute can be any of
  these types: `normal`, `vee`, `tee`, `dot`, `diamond`, `box`, `curve`,
  `icurve`, `inv`, `crow`, or `none`.

- arrowtail:

  The type of arrowtail to use. The `style` attribute can any of these
  types: `normal`, `vee`, `tee`, `dot`, `diamond`, `box`, `curve`,
  `icurve`, `inv`, `crow`, or `none`.

- fontname:

  The name of the system font that will be used for any edge text.

- fontsize:

  The point size of the font used for any edge text.

- fontcolor:

  The color used for any edge text. Can be an X11 color or a hexadecimal
  color code.

- len:

  The preferred edge length for an edge, in inches. Default value is
  `1.0`.

- tooltip:

  Text for a tooltip that appears when hovering over an edge. If text is
  not provided, then the default tooltip text will provide the edge
  definition (i.e., `[id]->[id] or [id]--[id]`).

- URL:

  A URL to associate with an edge. Upon rendering the plot, clicking
  edges with any associated URLs will open the URL in the default
  browser.

- label:

  The label text associated with the edge. This text will appear near
  the center of the edge.

- labelfontname:

  The name of the system font that will be used for the `headlabel` and
  the `taillabel` label text. If not set, the `fontname` value will
  instead be used.

- labelfontsize:

  The point size of the font used for the `headlabel` and the
  `taillabel` label text. If not set, the `fontsize` value will instead
  be used.

- labelfontcolor:

  The color used for the label text of the `headlabel` and the
  `taillabel` label text. If not set, the `fontcolor` value will instead
  be used. Can be an X11 color or a hexadecimal color code.

- labeltooltip:

  Text for a tooltip that will appear when hovering over the main label
  of an edge (if label text provided in the `label` edge attribute). If
  text is not provided and an edge label is visible, then the default
  tooltip text will provide the edge definition (i.e.,
  `[id]->[id] or [id]--[id]`).

- labelURL:

  A URL to associate with edge label text. Upon rendering the plot,
  clicking edge labels with any associated URLs will open the URL in the
  default browser.

- edgetooltip:

  This option provides a means to specify a tooltip with only the
  non-label parts of an edge. If this is defined, the value overrides
  any `tooltip` defined for the edge. This tooltip text is when hovering
  along the edge (even near the head or tail node) unless overridden by
  a `headtooltip` or `tailtooltip` value.

- edgeURL:

  This option provides a means to specify a URL with only the non-label
  parts of an edge. If this is defined, the value overrides any `URL`
  defined for the edge. This URL is used along the edge (even near the
  head or tail node) unless overridden by a `headURL` or `tailURL`
  value.

- dir:

  An optional direction type. Normally, for directed graphs, this is
  `forward` and needn't be set. For undirected graphs, this would be
  `none` and again no explicit setting is required. However, one can
  also use the `back` or `both` options. The `back` option draws an
  arrowhead in the reverse direction of an edge. The `both` option draws
  two arrowheads. When using any of these options in such an explicit
  manner, the `head...` and `tail...` edge attributes allow control over
  aesthetic edge attributes in either side of the edge.

- headtooltip:

  This option provides a means to specify a tooltip that can be
  displayed by hovering over the part of an edge that is adjacent to
  incoming node (see the `tooltip` argument for further details).

- headURL:

  This option provides a means to specify a URL that can be accessed by
  clicking the part of an edge that is adjacent to incoming node (see
  the `URL` argument for further details).

- headclip:

  If `TRUE` (the default behavior), then the head of the affected edge
  is clipped to the node boundary. Using `FALSE` places the head of the
  outgoing edge at the center of its node.

- headlabel:

  This option provides a means to display a label near the part of an
  edge that is adjacent to incoming node (see the `label` argument for
  further details).

- headport:

  Allows one to specify which compass position on the incoming node the
  head of the edge will alight. Options are `n`, `ne`, `e`, `se`, `s`,
  `sw`, `w`, and `nw`.

- tailtooltip:

  This option provides a means to specify a tooltip that can be
  displayed by hovering over the part of an edge that is adjacent to
  outgoing node (see the `tooltip` argument for further details).

- tailURL:

  This option provides a means to specify a URL that can be accessed by
  clicking the part of an edge that is adjacent to outgoing node (see
  the `URL` argument for further details).

- tailclip:

  If `TRUE` (the default behavior), then the tail of the affected edge
  is clipped to the node boundary. Using `FALSE` places the tail of the
  outgoing edge at the center of its node.

- taillabel:

  This option provides a means to display a label near the part of an
  edge that is adjacent to outgoing node (see the `label` argument for
  further details).

- tailport:

  Allows one to specify which compass position on the outgoing node the
  tail of the edge will be emitted from. Options are `n`, `ne`, `e`,
  `se`, `s`, `sw`, `w`, and `nw`.

- decorate:

  If `TRUE` then attach any edge label to the edge line via a 2-segment
  polyline, underlining the label text and partially overlapping the
  edge line.

## See also

Other aesthetics:
[`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md),
[`node_edge_aes_data`](https://rich-iannone.github.io/DiagrammeR/reference/node_edge_aes_data.md)

## Examples

``` r
# Create a new graph and add
# a path with several edge
# aesthetic attributes
graph <-
  create_graph() |>
  add_path(
    n = 3,
    type = "path",
    edge_aes = edge_aes(
      style = "dot",
      color = c("red", "blue")))

# View the graph's internal
# node data frame; the node
# aesthetic attributes have
# been inserted
graph |>
  get_edge_df()
#>   id from to  rel style color
#> 1  1    1  2 <NA>   dot   red
#> 2  2    2  3 <NA>   dot  blue
```
