#' Export a graph to various file formats
#' @description Export a graph to a variety of file
#' formats such as PNG, PDF, SVG, and PostScript.
#' @param graph a graph object.
#' @param file_name the name of the exported file
#' (including it's extension).
#' @param file_type the type of file to be exported.
#' Options are: \code{png}, \code{pdf}, \code{svg},
#' and \code{ps}.
#' @param width output width in pixels or \code{NULL}
#' for default.
#' @param height output height in pixels or \code{NULL}
#' for default.
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(DiagrammeRsvg)
#'
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = c("a", "b", "c", "d"),
#'     type = c("A", "A", "Z", "Z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = c("a", "b", "c"),
#'     to = c("d", "c", "a"),
#'     rel = c("A", "Z", "A"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Create a PDF file for the graph (`graph.pdf`)
#' graph %>% export_graph("graph.pdf")
#'
#' # Create a PNG file for the graph (`mypng`)
#' graph %>%
#' export_graph(
#'   file_name = "mypng",
#'   file_type = "PNG")
#' }
#' @import rsvg DiagrammeRsvg
#' @importFrom utils installed.packages
#' @export export_graph

export_graph <- function(graph,
                         file_name = NULL,
                         file_type = NULL,
                         width = NULL,
                         height = NULL) {

  if (!("DiagrammeRsvg" %in%
        rownames(utils::installed.packages()))) {
    stop("To use this function, please install the `DiagrammeRsvg` package using `devtools::install_github('rich-iannone/DiagrammeRsvg')")
  }

  # If no `file_name` or `file_type` provided, default to
  # writing a PDF with a unique `file_name` value based
  # on user's current date/time
  if (is.null(file_name) & is.null(file_type)) {
    file_name <-
      paste0("graph_",
             format(Sys.time(),
                    "%Y_%m_%d__%H_%M_%S"), ".pdf")
    file_type <- "pdf"
  }

  # If `file_name` provided but `file_type` is not, infer
  # the output file type based on the extension; if no
  # extension provided, default to PDF export
  if (is.null(file_type) & !is.null(file_name)) {
    if (grepl("\\.", file_name)) {
      file_type <-
        gsub(".*\\.([A-Za-z])", "\\1", file_name)
    } else {
      file_name <- paste0(file_name, ".pdf")
      file_type <- "pdf"
    }
  }

  if (file_type == "PNG" | file_type == "png") {
    rsvg_png(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(graph$dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "PDF" | file_type == "pdf") {
    rsvg_pdf(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(graph$dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "SVG" | file_type == "svg") {
    rsvg_svg(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(graph$dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "PS" | file_type == "ps") {
    rsvg_ps(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(graph$dot_code))),
      file = file_name,
      width = width,
      height = height)
  }
}
