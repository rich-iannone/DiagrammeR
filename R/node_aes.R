#' Insert node aesthetic attributes during node creation
#'
#' This helper function should be invoked to provide values for the namesake
#'   \code{node_aes} argument, which is present in any function where nodes are
#'   created.
#' @param shape the shape to use for the node. Some possible \code{shape} types
#'   include: \code{circle}, \code{rectangle}, \code{triangle},
#'   \code{plaintext}, \code{square}, and \code{polygon}.
#' @param style the node line style. The \code{style} types that can be used
#'   are: \code{filled}, \code{invisible}, \code{diagonals}, \code{rounded},
#'   \code{dashed}, \code{dotted}, \code{solid}, and \code{bold}.
#' @param penwidth the thickness of the stroke line (in pt units) for the node
#'   shape. The default value is \code{1.0}.
#' @param color the color of the node's outline. Can be any of the named colors
#'   that R knows about (obtained using the \code{colors()} function), or, a
#'   hexadecimal color code.
#' @param fillcolor the color with which to fill the shape of the node. Can be
#'   any of the named colors that R knows about (obtained using the
#'   \code{colors()} function), or, a hexadecimal color code.
#' @param fontname the name of the system font that will be used for any node
#'   text.
#' @param fontsize the point size of the font used for any node text.
#' @param fontcolor the color used for any node text. Can be any of the named
#'   colors that R knows about (obtained using the \code{colors()} function),
#'   or, a hexadecimal color code.
#' @param peripheries the repeated number of node shapes (of increasing size) to
#'   draw at the node perhipery.
#' @param height the height of the node shape, in inches. The default value is
#'   \code{0.5} whereas the minimum value is \code{0.02}. This is understood as
#'   the initial, minimum height of the node. If \code{fixedsize} is set to
#'   \code{TRUE}, this will be the final height of the node. Otherwise, if the
#'   node label requires more height to fit, the node's height will be increased
#'   to contain the label.
#' @param width the width of the node shape, in inches. The default value is
#'   \code{0.5} whereas the minimum value is \code{0.02}. This is understood as
#'   the initial, minimum width of the node. If \code{fixedsize} is set to
#'   \code{TRUE}, this will be the final width of the node. Otherwise, if the
#'   node label requires more width to fit, the node's width will be increased
#'   to contain the label.
#' @param x the fixed position of the node in the x direction. Any integer-based
#'   or floating point value will be accepted.
#' @param y the fixed position of the node in the y direction. Any integer-based
#'   or floating point value will be accepted.
#' @param group the node group.
#' @param tooltip text for a node tooltip.
#' @param xlabel External label for a node. The label will be placed outside of
#'   the node but near it. These labels are added after all nodes and edges have
#'   been placed. The labels will be placed so that they do not overlap any node
#'   or label. This means it may not be possible to place all of them.
#' @param URL a URL to associate with a node. Upon rendering the plot, clicking
#'   nodes with any associated URLs will open the URL in the default browser.
#' @param sides when using the shape \code{polygon}, this value will provide
#'   the number of sides for that polygon.
#' @param orientation this is the angle, in degrees, that is used to rotate
#'   nodes that have a \code{shape} of \code{polygon}. Not that for any of the
#'   polygon shapes (set by the \code{sides} node attribute), a value for
#'   \code{orientation} that is \code{0} results in a flat base.
#' @param skew a \code{0-1} value that will result in the node shape being
#'   skewed to the right (from bottom to top). A value in the range \code{0} to
#'   \code{-1} will skew the shape to the left.
#' @param distortion a distortion factor that is used only when a \code{shape}
#'   of \code{polygon} is used. A \code{0-1} value will increasingly result in
#'   the top part of the node polygon shape to be larger than the bottom. Moving
#'   from \code{0} toward \code{-1} will result in the opposite distortion
#'   effect.
#' @param gradientangle the path angle for the node color fill gradient.
#' @param fixedsize if set to \code{FALSE}, the size of a node is determined by
#'   smallest width and height needed to contain its label, if any, with a
#'   margin specified by the \code{margin} node attribute. The width and height
#'   must also be at least as large as the sizes specified by the \code{width}
#'   and \code{height} node attributes, which specify the minimum values. If set
#'   to \code{TRUE}, the node size is entirely specified by the values of the
#'   \code{width} and \code{height} node attributes (i.e., the node is not
#'   expanded in size to contain the text label).
#' @param labelloc sets the vertical placement of labels for nodes and clusters.
#'   This attribute is used only when the height of the node is larger than the
#'   height of its label. The \code{labelloc} node attribute can be set to
#'   either \code{t} (top), \code{c} (center), or \code{b} (bottom). By default,
#'   the label is vertically centered.
#' @param margin sets the amount of space around the node's label. By default,
#'   the value is \code{0.11,0.055}.
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
#'       y = c(4, -1, 3)))
#'
#' # View the graph's internal
#' # node data frame; the node
#' # aesthetic attributes have
#' # been inserted
#' graph %>% get_node_df()
#' @importFrom purrr map_chr
#' @export
node_aes <- function(shape = NULL,
                     style = NULL,
                     penwidth = NULL,
                     color = NULL,
                     fillcolor = NULL,
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
