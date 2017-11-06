#' Insert edge aesthetic attributes during edge creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{edge_aes} argument, which is present
#' in any function where edges are created.
#' @param style the edge line style. The
#' \code{style} types that can be used are
#' \code{solid}, \code{bold}, \code{dashed},
#' \code{dotted}, \code{tapered}, and
#' \code{invisible}.
#' @param penwidth the thickness of the stroke
#' line for the edge itself.
#' @param color the color of the edge. Can be
#' an X11 color or a hexadecimal color code.
#' @param arrowsize a scaling factor for
#' arrowheads. The default value is \code{1.0}
#' and the minimum is \code{0}.
#' @param arrowhead the type of arrowhead to use.
#' The \code{style} attribute can either any of
#' these types: \code{normal}, \code{vee},
#' \code{tee}, \code{dot}, \code{diamond},
#' \code{box}, \code{curve}, \code{icurve},
#' \code{inv}, \code{crow}, or \code{none}.
#' @param arrowtail the type of arrowtail to use.
#' The \code{style} attribute can either any of
#' these types: \code{normal}, \code{vee},
#' \code{tee}, \code{dot}, \code{diamond},
#' \code{box}, \code{curve}, \code{icurve},
#' \code{inv}, \code{crow}, or \code{none}.
#' @param fontname the name of the
#' system font that will be used for any edge
#' text.
#' @param fontsize the point size of
#' the font used for any edge text.
#' @param fontcolor the color used
#' for any edge text. Can be an X11 color or a
#' hexadecimal color code.
#' @param len the preferred edge length for
#' an edge, in inches. Default value is
#' \code{1.0}.
#' @param tooltip text for a tooltip that
#' appears when hovering over an edge. If text
#' is not provided, then the default tooltip
#' text will provide the edge definition (i.e.,
#' \code{[id]->[id] or [id]--[id]}).
#' @param URL a URL to associate with an edge.
#' Upon rendering the plot, clicking edges
#' with any associated URLs will open the
#' URL in the default browser.
#' @param label the label text associated
#' with the edge. This text will appear near
#' the center of the edge.
#' @param labelfontname the name of the
#' system font that will be used for the
#' \code{headlabel} and the
#' \code{taillabel} label text. If not
#' set, the \code{fontname} value will
#' instead be used.
#' @param labelfontsize the point size of
#' the font used for the \code{headlabel}
#' and the \code{taillabel} label text.
#' If not set, the \code{fontsize} value
#' will instead be used.
#' @param labelfontcolor the color used
#' for the label text of the
#' \code{headlabel} and the
#' \code{taillabel} label text. If not
#' set, the \code{fontcolor} value will
#' instead be used. Can be an X11 color
#' or a hexadecimal color code.
#' @param labeltooltip text for a tooltip
#' that will appear when hovering over the
#' main label of an edge (if label text
#' provided in the \code{label} edge
#' attribute). If text is not provided and
#' an edge label is visible, then the
#' default tooltip text will provide the
#' edge definition (i.e., \code{[id]->[id]
#' or [id]--[id]}).
#' @param labelURL a URL to associate with
#' edge label text. Upon rendering the plot,
#' clicking edge labels with any associated
#' URLs will open the URL in the default
#' browser.
#' @param edgetooltip edgetooltip
#' @param edgeURL edgeURL
#' @param dir an optional direction
#' type. Normally, for directed
#' graphs, this is \code{forward}
#' and needn't be set. For undirected
#' graphs, this would be \code{none}
#' and again no explicit setting is
#' required. However, one can also use
#' the \code{back} or \code{both}
#' options. The \code{back} option
#' draws an arrowhead in the reverse
#' direction of an edge. The
#' \code{both} option draws two
#' arrowheads. When using any of
#' these options in such an explicit
#' manner, the \code{head...} and
#' \code{tail...} edge attributes
#' allow control over aesthetic edge
#' attributes in either side of the
#' edge.
#' @param headtooltip headtooltip
#' @param headURL headURL
#' @param headclip headclip
#' @param headlabel headlabel
#' @param headport headport
#' @param tailtooltip tailtooltip
#' @param tailURL tailURL
#' @param tailclip tailclip
#' @param taillabel taillabel
#' @param tailport tailport
#' @param weight weight
#' @param constraint constraint
#' @param decorate decorate
#' @param lhead lhead
#' @param ltail ltail
#' @param samehead samehead
#' @param sametail sametail
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
#' #>   id from to  rel style color
#' #> 1  1    1  2 <NA>   dot   red
#' #> 2  2    2  3 <NA>   dot  blue
#' @importFrom purrr map_chr
#' @export edge_aes

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
                     weight = NULL,
                     constraint = NULL,
                     decorate = NULL,
                     lhead = NULL,
                     ltail = NULL,
                     samehead = NULL,
                     sametail = NULL) {

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
      dir = dir,
      weight = weight,
      constraint = constraint,
      decorate = decorate,
      lhead = lhead,
      ltail = ltail,
      samehead = samehead,
      sametail = sametail)

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
