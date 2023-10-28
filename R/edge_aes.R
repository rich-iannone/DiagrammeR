#' Insert edge aesthetic attributes during edge creation
#'
#' @description
#'
#' This helper function should be invoked to provide values for the namesake
#' `edge_aes` argument, which is present in any function where edges are
#' created.
#'
#' @param style The edge line style. The `style` types that can be used are
#'   `solid`, `bold`, `dashed`, `dotted`, `tapered`, and `invisible`.
#' @param penwidth The thickness of the stroke line for the edge itself.
#' @param color The color of the edge. Can be an X11 color or a hexadecimal
#'   color code.
#' @param arrowsize A scaling factor for arrowheads. The default value is `1.0`
#'   and the minimum is `0`.
#' @param arrowhead The type of arrowhead to use. The `style` attribute can be
#'   any of these types: `normal`, `vee`, `tee`, `dot`, `diamond`, `box`,
#'   `curve`, `icurve`, `inv`, `crow`, or `none`.
#' @param arrowtail The type of arrowtail to use. The `style` attribute can any
#'   of these types: `normal`, `vee`, `tee`, `dot`, `diamond`, `box`, `curve`,
#'   `icurve`, `inv`, `crow`, or `none`.
#' @param fontname The name of the system font that will be used for any edge
#'   text.
#' @param fontsize The point size of the font used for any edge text.
#' @param fontcolor The color used for any edge text. Can be an X11 color or a
#'   hexadecimal color code.
#' @param len The preferred edge length for an edge, in inches. Default value is
#'   `1.0`.
#' @param tooltip Text for a tooltip that appears when hovering over an edge. If
#'   text is not provided, then the default tooltip text will provide the edge
#'   definition (i.e., `[id]->[id] or [id]--[id]`).
#' @param URL A URL to associate with an edge. Upon rendering the plot, clicking
#'   edges with any associated URLs will open the URL in the default browser.
#' @param label The label text associated with the edge. This text will appear
#'   near the center of the edge.
#' @param labelfontname The name of the system font that will be used for the
#'   `headlabel` and the `taillabel` label text. If not set, the `fontname`
#'   value will instead be used.
#' @param labelfontsize The point size of the font used for the `headlabel` and
#'   the `taillabel` label text. If not set, the `fontsize` value will instead
#'   be used.
#' @param labelfontcolor The color used for the label text of the `headlabel`
#'   and the `taillabel` label text. If not set, the `fontcolor` value will
#'   instead be used. Can be an X11 color or a hexadecimal color code.
#' @param labeltooltip Text for a tooltip that will appear when hovering over
#'   the main label of an edge (if label text provided in the `label` edge
#'   attribute). If text is not provided and an edge label is visible, then the
#'   default tooltip text will provide the edge definition (i.e., `[id]->[id] or
#'   [id]--[id]`).
#' @param labelURL A URL to associate with edge label text. Upon rendering the
#'   plot, clicking edge labels with any associated URLs will open the URL in
#'   the default browser.
#' @param edgetooltip This option provides a means to specify a tooltip with
#'   only the non-label parts of an edge. If this is defined, the value
#'   overrides any `tooltip` defined for the edge. This tooltip text is when
#'   hovering along the edge (even near the head or tail node) unless overridden
#'   by a `headtooltip` or `tailtooltip` value.
#' @param edgeURL This option provides a means to specify a URL with only the
#'   non-label parts of an edge. If this is defined, the value overrides any
#'   `URL` defined for the edge. This URL is used along the edge (even near the
#'   head or tail node) unless overridden by a `headURL` or `tailURL` value.
#' @param dir An optional direction type. Normally, for directed graphs, this is
#'   `forward` and needn't be set. For undirected graphs, this would be `none`
#'   and again no explicit setting is required. However, one can also use the
#'   `back` or `both` options. The `back` option draws an arrowhead in the
#'   reverse direction of an edge. The `both` option draws two arrowheads. When
#'   using any of these options in such an explicit manner, the `head...` and
#'   `tail...` edge attributes allow control over aesthetic edge attributes in
#'   either side of the edge.
#' @param headtooltip This option provides a means to specify a tooltip that can
#'   be displayed by hovering over the part of an edge that is adjacent to
#'   incoming node (see the `tooltip` argument for further details).
#' @param headURL This option provides a means to specify a URL that can be
#'   accessed by clicking the part of an edge that is adjacent to incoming node
#'   (see the `URL` argument for further details).
#' @param headclip If `TRUE` (the default behavior), then the head of the
#'   affected edge is clipped to the node boundary. Using `FALSE` places the
#'   head of the outgoing edge at the center of its node.
#' @param headlabel This option provides a means to display a label near the
#'   part of an edge that is adjacent to incoming node (see the `label` argument
#'   for further details).
#' @param headport Allows one to specify which compass position on the incoming
#'   node the head of the edge will alight. Options are `n`, `ne`, `e`, `se`,
#'   `s`, `sw`, `w`, and `nw`.
#' @param tailtooltip This option provides a means to specify a tooltip that can
#'   be displayed by hovering over the part of an edge that is adjacent to
#'   outgoing node (see the `tooltip` argument for further details).
#' @param tailURL This option provides a means to specify a URL that can be
#'   accessed by clicking the part of an edge that is adjacent to outgoing node
#'   (see the `URL` argument for further details).
#' @param tailclip If `TRUE` (the default behavior), then the tail of the
#'   affected edge is clipped to the node boundary. Using `FALSE` places the
#'   tail of the outgoing edge at the center of its node.
#' @param taillabel This option provides a means to display a label near the
#'   part of an edge that is adjacent to outgoing node (see the `label` argument
#'   for further details).
#' @param tailport Allows one to specify which compass position on the outgoing
#'   node the tail of the edge will be emitted from. Options are `n`, `ne`, `e`,
#'   `se`, `s`, `sw`, `w`, and `nw`.
#' @param decorate If `TRUE` then attach any edge label to the edge line via a
#'   2-segment polyline, underlining the label text and partially overlapping
#'   the edge line.
#'
#' @examples
#' # Create a new graph and add
#' # a path with several edge
#' # aesthetic attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     type = "path",
#'     edge_aes = edge_aes(
#'       style = "dot",
#'       color = c("red", "blue")))
#'
#' # View the graph's internal
#' # node data frame; the node
#' # aesthetic attributes have
#' # been inserted
#' graph %>%
#'   get_edge_df()
#'
#' @family aesthetics
#'
#' @export
edge_aes <- function(style = NULL,
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
                     decorate = NULL) {

  # Collect vectors of node aesthetic
  # attribute values into a list object
  attr_values <-
    list(
      style = style,
      penwidth = penwidth,
      color = color,
      arrowsize = arrowsize,
      arrowhead = arrowhead,
      arrowtail = arrowtail,
      fontname = fontname,
      fontsize = fontsize,
      fontcolor = fontcolor,
      len = len,
      tooltip = tooltip,
      URL = URL,
      label = label,
      labelfontname = labelfontname,
      labelfontsize = labelfontsize,
      labelfontcolor = labelfontcolor,
      labeltooltip = labeltooltip,
      labelURL = labelURL,
      edgetooltip = edgetooltip,
      edgeURL = edgeURL,
      dir = dir,
      headtooltip = headtooltip,
      headURL = headURL,
      headclip = headclip,
      headlabel = headlabel,
      headport = headport,
      tailtooltip = tailtooltip,
      tailURL = tailURL,
      tailclip = tailclip,
      taillabel = taillabel,
      tailport = tailport,
      decorate = decorate)

  non_null_attrs <-
    1:length(attr_values) %>%
    purrr::map_chr(.f = function(x) {
      if (!is.null(attr_values[[x]])) {
        attr_values[x] %>% names()
      } else {
        NA
      }
    })

  non_null_attrs <- non_null_attrs[which(!is.na(non_null_attrs))]

  attr_values[non_null_attrs]
}
