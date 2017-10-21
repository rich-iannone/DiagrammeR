#' Insert edge aesthetic attributes during edge creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{edge_aes} argument, which is present
#' in any function where edges are created.
#' @param style the type of arrowhead to use.
#' The \code{style} attribute can either any of
#' these types: \code{normal}, \code{vee},
#' \code{tee}, \code{dot}, \code{diamond},
#' \code{box}, \code{curve}, \code{icurve},
#' \code{inv}, \code{crow}, or \code{none}.
#' @param penwidth the thickness of the stroke
#' line for the edge itself.
#' @param color the color of the edge. Can be
#' an X11 color or a hexadecimal color code.
#' @param arrowsize a scaling factor for
#' arrowheads. The default value is \code{1.0}
#' and the minimum is \code{0}.
#' @param arrowhead arrowhead
#' @param arrowtail arrowtail
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
#' system font that will be used for any
#' edge label text.
#' @param labelfontsize labelfontsize
#' @param labelfontcolor labelfontcolor
#' @param labelangle labelangle
#' @param labeldistance labeldistance
#' @param labelfloat labelfloat
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
#' @param dir dir
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
                     dir = NULL,
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
