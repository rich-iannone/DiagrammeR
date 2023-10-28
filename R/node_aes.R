#' Insert node aesthetic attributes during node creation
#'
#' @description
#'
#' This helper function should be invoked to provide values for the namesake
#' `node_aes` argument, which is present in any function where nodes are
#' created.
#'
#' @param shape The shape to use for the node. Some possible `shape` types
#'   include: `circle`, `rectangle`, `triangle`, `plaintext`, `square`, and
#'   `polygon`.
#' @param style The node line style. The `style` types that can be used are:
#'   `filled`, `invisible`, `diagonals`, `rounded`, `dashed`, `dotted`, `solid`,
#'   and `bold`.
#' @param penwidth The thickness of the stroke line (in pt units) for the node
#'   shape. The default value is `1.0`.
#' @param color The color of the node's outline. Can be any of the named colors
#'   that R knows about (obtained using the `colors()` function), or, a
#'   hexadecimal color code.
#' @param fillcolor The color with which to fill the shape of the node. Can be
#'   any of the named colors that R knows about (obtained using the `colors()`
#'   function), or, a hexadecimal color code.
#' @param image A reference to an image location.
#' @param fontname The name of the system font that will be used for any node
#'   text.
#' @param fontsize The point size of the font used for any node text.
#' @param fontcolor The color used for any node text. Can be any of the named
#'   colors that R knows about (obtained using the `colors()` function), or, a
#'   hexadecimal color code.
#' @param peripheries The repeated number of node shapes (of increasing size) to
#'   draw at the node periphery.
#' @param height The height of the node shape, in inches. The default value is
#'   `0.5` whereas the minimum value is `0.02`. This is understood as the
#'   initial, minimum height of the node. If `fixedsize` is set to `TRUE`, this
#'   will be the final height of the node. Otherwise, if the node label requires
#'   more height to fit, the node's height will be increased to contain the
#'   label.
#' @param width The width of the node shape, in inches. The default value is
#'   `0.5` whereas the minimum value is `0.02`. This is understood as the
#'   initial, minimum width of the node. If `fixedsize` is set to `TRUE`, this
#'   will be the final width of the node. Otherwise, if the node label requires
#'   more width to fit, the node's width will be increased to contain the label.
#' @param x The fixed position of the node in the x direction. Any integer-based
#'   or floating point value will be accepted.
#' @param y The fixed position of the node in the y direction. Any integer-based
#'   or floating point value will be accepted.
#' @param group The node group.
#' @param tooltip Text for a node tooltip.
#' @param xlabel External label for a node. The label will be placed outside of
#'   the node but near it. These labels are added after all nodes and edges have
#'   been placed. The labels will be placed so that they do not overlap any node
#'   or label. This means it may not be possible to place all of them.
#' @param URL A URL to associate with a node. Upon rendering the plot, clicking
#'   nodes with any associated URLs will open the URL in the default browser.
#' @param sides When using the shape `polygon`, this value will provide the
#'   number of sides for that polygon.
#' @param orientation This is the angle, in degrees, that is used to rotate
#'   nodes that have a `shape` of `polygon`. Not that for any of the polygon
#'   shapes (set by the `sides` node attribute), a value for `orientation` that
#'   is `0` results in a flat base.
#' @param skew A `0-1` value that will result in the node shape being skewed to
#'   the right (from bottom to top). A value in the range `0` to `-1` will skew
#'   the shape to the left.
#' @param distortion A distortion factor that is used only when a `shape` of
#'   `polygon` is used. A `0-1` value will increasingly result in the top part
#'   of the node polygon shape to be larger than the bottom. Moving from `0`
#'   toward `-1` will result in the opposite distortion effect.
#' @param gradientangle The path angle for the node color fill gradient.
#' @param fixedsize If set to `FALSE`, the size of a node is determined by
#'   smallest width and height needed to contain its label, if any, with a
#'   margin specified by the `margin` node attribute. The width and height must
#'   also be at least as large as the sizes specified by the `width` and
#'   `height` node attributes, which specify the minimum values. If set to
#'   `TRUE`, the node size is entirely specified by the values of the `width`
#'   and `height` node attributes (i.e., the node is not expanded in size to
#'   contain the text label).
#' @param labelloc Sets the vertical placement of labels for nodes and clusters.
#'   This attribute is used only when the height of the node is larger than the
#'   height of its label. The `labelloc` node attribute can be set to either `t`
#'   (top), `c` (center), or `b` (bottom). By default, the label is vertically
#'   centered.
#' @param margin Sets the amount of space around the node's label. By default,
#'   the value is `0.11,0.055`.
#' @examples
#' # Create a new graph and add
#' # a path with several node
#' # aesthetic attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     type = "path",
#'     node_aes = node_aes(
#'       shape = "circle",
#'       x = c(1, 3, 2),
#'       y = c(4, -1, 3)
#'     )
#'   )
#'
#' # View the graph's internal
#' # node data frame; the node
#' # aesthetic attributes have
#' # been inserted
#' graph %>% get_node_df()
#'
#' # Create a new graph which is
#' # fully connected
#' graph <-
#'   create_graph() %>%
#'   add_full_graph(
#'     n = 4,
#'     node_data = node_data(value = 1:4),
#'     node_aes = node_aes(
#'       x = c(2, 1, 3, 2),
#'       y = c(3, 2, 2, 1)
#'     ),
#'     edge_aes = edge_aes(color = "blue")
#'   )
#' @family aesthetics
#' @export
node_aes <- function(shape = NULL,
                     style = NULL,
                     penwidth = NULL,
                     color = NULL,
                     fillcolor = NULL,
                     image = NULL,
                     #fa_icon = NULL,
                     fontname = NULL,
                     fontsize = NULL,
                     fontcolor = NULL,
                     peripheries = NULL,
                     height = NULL,
                     width = NULL,
                     x = NULL,
                     y = NULL,
                     group = NULL,
                     tooltip = NULL,
                     xlabel = NULL,
                     URL = NULL,
                     sides = NULL,
                     orientation = NULL,
                     skew = NULL,
                     distortion = NULL,
                     gradientangle = NULL,
                     fixedsize = NULL,
                     labelloc = NULL,
                     margin = NULL) {

  # Collect vectors of node aesthetic
  # attribute values into a list object
  attr_values <-
    list(
      shape = shape,
      style = style,
      penwidth = penwidth,
      color = color,
      fillcolor = fillcolor,
      image = image,
      #fa_icon = fa_icon,
      fontname = fontname,
      fontsize = fontsize,
      fontcolor = fontcolor,
      peripheries = peripheries,
      height = height,
      width = width,
      x = x,
      y = y,
      group = group,
      tooltip = tooltip,
      xlabel = xlabel,
      URL = URL,
      sides = sides,
      orientation = orientation,
      skew = skew,
      distortion = distortion,
      gradientangle = gradientangle,
      fixedsize = fixedsize,
      labelloc = labelloc,
      margin = margin)

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
