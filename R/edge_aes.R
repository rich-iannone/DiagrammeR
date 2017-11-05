#' Insert edge aesthetic attributes during edge creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{edge_aes} argument, which is present
#' in any function where edges are created.
#' @param style the edge line style. The
#' \code{style} types that can be used are
#' \code{solid}, \code{bold}, \code{dashed},
#' \code{dotted}, and \code{invisible}.
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
#' @param len len
#' @param minlen minlen
#' @param tooltip text for an edge tooltip.
#' @param URL a URL to associate with an edge.
#' Upon rendering the plot, clicking edges
#' with any associated URLs will open the
#' URL in the default browser.
#' @param layer layer
#' @param label the label text associated
#' with the edge.
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
#' @param labelangle when used along with
#' \code{labeldistance} edge attribute,
#' determines where the \code{headlabel}
#' (or \code{taillabel}) are placed with
#' respect to the edge's head (or tail)
#' using polar coordinates. The origin for
#' the coordinate system is the point
#' where the edge touches the node. Now
#' imagine a ray (of \code{0} degrees)
#' moving from the origin back along the
#' edge, parallel to the edge at the origin.
#' The angle, in degrees, specifies the
#' rotation from the \code{0} degree ray,
#' with positive angles moving
#' counterclockwise and negative angles
#' moving clockwise.
#' @param labeldistance a scaling factor
#' that adjusts the distance of the
#' \code{headlabel} (or, \code{taillabel})
#' from the head (or tail) node. Consider
#' that the distance is normally 10 points.
#' Any value of \code{labeldistance} will
#' effectively multiply that distance
#' value. The default value is \code{1.0}
#' and the minimum is \code{0}.
#' @param labelfloat if set to \code{TRUE},
#' this option allows edge labels to be less
#' constrained in position. That is, edge
#' labels may appear on top of other edges.
#' The default here is \code{FALSE}.
#' @param labeltooltip labeltooltip
#' @param labelhref labelhref
#' @param labelURL a URL to associate with
#' edge label text. Upon rendering the plot,
#' clicking edge labels with any associated
#' URLs will open the URL in the default
#' browser.
#' @param labeltarget labeltarget
#' @param edgetooltip edgetooltip
#' @param edgehref edgehref
#' @param edgeURL edgeURL
#' @param edgetarget edgetarget
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
#' @param headhref headhref
#' @param headURL headURL
#' @param headtarget headtarget
#' @param headclip headclip
#' @param headlabel headlabel
#' @param headport headport
#' @param tailtooltip tailtooltip
#' @param tailhref tailhref
#' @param tailURL tailURL
#' @param tailtarget tailtarget
#' @param tailclip tailclip
#' @param taillabel taillabel
#' @param tailport tailport
#' @param target target
#' @param weight weight
#' @param constraint constraint
#' @param decorate decorate
#' @param href href
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
                     minlen = NULL,
                     tooltip = NULL,
                     URL = NULL,
                     layer = NULL,
                     label = NULL,
                     labelfontname = NULL,
                     labelfontsize = NULL,
                     labelfontcolor = NULL,
                     labelangle = NULL,
                     labeldistance = NULL,
                     labelfloat = NULL,
                     labeltooltip = NULL,
                     labelhref = NULL,
                     labelURL = NULL,
                     labeltarget = NULL,
                     edgetooltip = NULL,
                     edgehref = NULL,
                     edgeURL = NULL,
                     edgetarget = NULL,
                     dir = NULL,
                     headtooltip = NULL,
                     headhref = NULL,
                     headURL = NULL,
                     headtarget = NULL,
                     headclip = NULL,
                     headlabel = NULL,
                     headport = NULL,
                     tailtooltip = NULL,
                     tailhref = NULL,
                     tailURL = NULL,
                     tailtarget = NULL,
                     tailclip = NULL,
                     taillabel = NULL,
                     tailport = NULL,
                     target = NULL,
                     weight = NULL,
                     constraint = NULL,
                     decorate = NULL,
                     href = NULL,
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
      minlen = minlen,
      tooltip = tooltip,
      URL = URL,
      layer = layer,
      label = label,
      labelfontname = labelfontname,
      labelfontsize = labelfontsize,
      labelfontcolor = labelfontcolor,
      labelangle = labelangle,
      labeldistance = labeldistance,
      labelfloat = labelfloat,
      labeltooltip = labeltooltip,
      labelhref = labelhref,
      labelURL = labelURL,
      labeltarget = labeltarget,
      edgetooltip = edgetooltip,
      edgehref = edgehref,
      edgeURL = edgeURL,
      edgetarget = edgetarget,
      headtooltip = headtooltip,
      headhref = headhref,
      headURL = headURL,
      headtarget = headtarget,
      headclip = headclip,
      headlabel = headlabel,
      headport = headport,
      tailtooltip = tailtooltip,
      tailhref = tailhref,
      tailURL = tailURL,
      tailtarget = tailtarget,
      tailclip = tailclip,
      taillabel = taillabel,
      tailport = tailport,
      dir = dir,
      target = target,
      weight = weight,
      constraint = constraint,
      decorate = decorate,
      href = href,
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
