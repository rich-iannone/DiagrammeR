#' Insert node aesthetic attributes during node creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{node_aes} argument, which is present
#' in any function where nodes are created.
#' @param shape the shape to use for the node.
#' @param style the node line style.
#' @param penwidth the thickness of the stroke
#' line for the node shape.
#' @param color color the color of the node's
#' outline. Can be an X11 color or a hexadecimal
#' color code.
#' @param fillcolor fillcolor the color with which
#' to fill the shape of the node. Can be an X11
#' color or a hexadecimal color code.
#' @param fontname fontname the name of the
#' system font that will be used for any node
#' text.
#' @param fontsize fontsize the point size of
#' the font used for any node text.
#' @param fontcolor fontcolor the color used
#' for any node text. Can be an X11 color or a
#' hexadecimal color code.
#' @param height the height of the shape.
#' @param width the width of the shape.
#' @param x the fixed position of the node in
#' the x direction. Any integer-based or
#' floating point value will be accepted.
#' @param y the fixed position of the node in
#' the y direction. Any integer-based or
#' floating point value will be accepted.
#' @param group the node group.
#' @param tooltip text for a node tooltip.
#' @param URL a URL to associate with a node.
#' Upon rendering the plot, clicking nodes
#' with any associated URLs will open the
#' URL in the default browser.
#' @param distortion the level of distortion
#' for the node.
#' @param sides sides when using the shape
#' \code{polygon}, this value will provide
#' the number of sides for that polygon.
#' @param skew a \code{0-1} value that will
#' result in the node shape being skewed
#' to the right (from bottom to top). A
#' value in the range \code{0} to \code{-1}
#' will skew the shape to the left.
#' @param peripheries the repeated number
#' of node shapes (of increasing size) to
#' draw at the node perhipery.
#' @param gradientangle the path angle for
#' the node color fill gradient.
#' @param fixedsize fixedsize
#' @param labelloc labelloc
#' @param margin margin
#' @param orientation orientation
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
#' graph %>%
#'   get_node_df()
#' #>   id type label  shape x  y
#' #> 1  1 path     1 circle 1  4
#' #> 2  2 path     2 circle 3 -1
#' #> 3  3 path     3 circle 2  3
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @export node_aes

node_aes <- function(shape = NULL,
                     style = NULL,
                     penwidth = NULL,
                     color = NULL,
                     fillcolor = NULL,
                     fontname = NULL,
                     fontsize = NULL,
                     fontcolor = NULL,
                     height = NULL,
                     width = NULL,
                     x = NULL,
                     y = NULL,
                     group = NULL,
                     tooltip = NULL,
                     URL = NULL,
                     distortion = NULL,
                     sides = NULL,
                     skew = NULL,
                     peripheries = NULL,
                     gradientangle = NULL,
                     fixedsize = NULL,
                     labelloc = NULL,
                     margin = NULL,
                     orientation = NULL) {

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
      height = height,
      width = width,
      x = x,
      y = y,
      group = group,
      tooltip = tooltip,
      URL = URL,
      distortion = distortion,
      sides = sides,
      skew = skew,
      peripheries = peripheries,
      gradientangle = gradientangle,
      label = label,
      fixedsize = fixedsize,
      labelloc = labelloc,
      margin = margin,
      orientation = orientation)

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
