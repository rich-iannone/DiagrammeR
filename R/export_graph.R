#' Export a graph to various image formats
#'
#' @description
#'
#' Export a graph to a variety of image formats such as PNG, PDF, SVG, and
#' PostScript.
#'
#' @inheritParams render_graph
#' @param file_name The name of the exported file (including it's extension).
#' @param file_type The type of file to be exported. Options for graph files
#'   are: `png`, `pdf`, `svg`, and `ps`.
#' @param title An optional title for the output graph.
#' @param width Output width in pixels or `NULL` for default. Only useful for
#'   export to image file formats `png`, `pdf`, `svg`, and `ps`.
#' @param height Output height in pixels or `NULL` for default. Only useful for
#'   export to image file formats `png`, `pdf`, `svg`, and `ps`.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'     add_path(
#'       n = 5,
#'       edge_aes = edge_aes(
#'         arrowhead = c(
#'           "normal", "vee",
#'           "tee", "dot"
#'         ),
#'         color = c(
#'         "red", "blue",
#'         "orange", "purple"
#'         )
#'       )
#'     )
#'
#' # Create a PDF file for
#' # the graph (`graph.pdf`)
#' # graph %>%
#' #   export_graph(
#' #     file_name = "graph.pdf",
#' #     title = "Simple Graph"
#' #   )
#'
#' # Create a PNG file for
#' # the graph (`mypng.png`)
#' # graph %>%
#' #   export_graph(
#' #     file_name = "mypng.png",
#' #     file_type = "PNG"
#' #   )
#'
#' @family Display and Save
#'
#' @export
export_graph <- function(
    graph,
    file_name = NULL,
    file_type = NULL,
    title = NULL,
    width = NULL,
    height = NULL
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If no `file_name` or `file_type` provided, default to
  # writing a PDF with a unique `file_name` value based
  # on user's current date/time
  if (is.null(file_name) & is.null(file_type)) {
    file_name <-
      paste0("graph_", format(Sys.time(), "%Y_%m_%d__%H_%M_%S"), ".pdf")
    file_type <- "pdf"
  }

  # If `file_name` provided but `file_type` is not, infer
  # the output file type based on the extension; if no
  # extension provided, default to PDF export
  if (is.null(file_type) & !is.null(file_name)) {
    if (grepl("\\.", file_name)) {
      file_type <- gsub(".*\\.([A-Za-z])", "\\1", file_name)
    } else {
      file_name <- paste0(file_name, ".pdf")
      file_type <- "pdf"
    }
  }

  if (file_type == "PNG" | file_type == "png") {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PNG file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!requireNamespace("rsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PNG file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <- add_global_graph_attrs(graph, "label", title, "graph")
      graph <- add_global_graph_attrs(graph, "labelloc", "t", "graph")
      graph <- add_global_graph_attrs(graph, "labeljust", "c", "graph")
      graph <- add_global_graph_attrs(graph, "fontname", "Helvetica", "graph")
      graph <- add_global_graph_attrs(graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PNG file in the working directory
    rsvg::rsvg_png(
      charToRaw(DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height
    )
  }

  if (file_type == "PDF" | file_type == "pdf") {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PDF file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')"))
    }

    # Stop function if `rsvg` package is not available
    if (!requireNamespace("rsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PDF file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {
      graph <- add_global_graph_attrs(graph, "label", title, "graph")
      graph <- add_global_graph_attrs(graph, "labelloc", "t", "graph")
      graph <- add_global_graph_attrs(graph, "labeljust", "c", "graph")
      graph <- add_global_graph_attrs(graph, "fontname", "Helvetica", "graph")
      graph <- add_global_graph_attrs(graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PDF file in the working directory
    rsvg::rsvg_pdf(
      charToRaw(DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height
    )
  }

  if (file_type == "SVG" | file_type == "svg") {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce an SVG file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!requireNamespace("rsvg", quietly = TRUE)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce an SVG file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <- add_global_graph_attrs(graph, "label", title, "graph")
      graph <- add_global_graph_attrs(graph, "labelloc", "t", "graph")
      graph <- add_global_graph_attrs(graph, "labeljust", "c", "graph")
      graph <- add_global_graph_attrs(graph, "fontname", "Helvetica", "graph")
      graph <- add_global_graph_attrs(graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce an SVG file in the working directory
    rsvg::rsvg_svg(
      charToRaw(DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height
    )
  }

  if ((file_type == "PS" | file_type == "ps") &&
    requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
    requireNamespace("rsvg", quietly = TRUE)) {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!("DiagrammeRsvg" %in% rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PS file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!("rsvg" %in% rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PS file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <- add_global_graph_attrs(graph, "label", title, "graph")
      graph <- add_global_graph_attrs(graph, "labelloc", "t", "graph")
      graph <- add_global_graph_attrs(graph, "labeljust", "c", "graph")
      graph <- add_global_graph_attrs(graph, "fontname", "Helvetica", "graph")
      graph <- add_global_graph_attrs(graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PS file in the working directory
    rsvg::rsvg_ps(
      charToRaw(DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height
    )
  }
}
