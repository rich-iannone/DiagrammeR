#' Export a graph to various file formats
#' @description Export a graph to a variety of file
#' formats, including image formats such as PNG, PDF,
#' SVG, and PostScript, and graph file formats such
#' as GEXF.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param file_name the name of the exported file
#' (including it's extension).
#' @param file_type the type of file to be exported.
#' Options for graph files are: \code{png}, \code{pdf},
#' \code{svg}, and \code{ps}. Options for graph file
#' formats are: \code{gexf}.
#' @param title an optional title for the output graph.
#' @param width output width in pixels or \code{NULL}
#' for default. Only useful for export to image file
#' formats \code{png}, \code{pdf}, \code{svg}, and
#' \code{ps}.
#' @param height output height in pixels or \code{NULL}
#' for default. Only useful for export to image file
#' formats \code{png}, \code{pdf}, \code{svg}, and
#' \code{ps}.
#' @examples
#' \dontrun{
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'     add_path(
#'       n = 5,
#'       edge_aes = edge_aes(
#'         arrowhead = c(
#'           "normal", "vee",
#'           "tee", "dot"),
#'         color = c(
#'         "red", "blue",
#'         "orange", "purple")))
#'
#' # Create a PDF file for
#' # the graph (`graph.pdf`)
#' graph %>%
#'   export_graph(
#'     file_name = "graph.pdf",
#'     title = "Simple Graph")
#'
#' # Create a PNG file for
#' # the graph (`mypng.png`)
#' graph %>%
#'   export_graph(
#'     file_name = "mypng.png",
#'     file_type = "PNG")
#' }
#' @importFrom rgexf write.gexf
#' @importFrom utils installed.packages
#' @importFrom igraph V E ecount ends vertex_attr_names edge_attr_names
#' @importFrom igraph graph_attr_names vertex_attr edge_attr graph_attr
#' @export export_graph

export_graph <- function(graph,
                         file_name = NULL,
                         file_type = NULL,
                         title = NULL,
                         width = NULL,
                         height = NULL) {

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

  if (file_type == "PNG" | file_type == "png" &&
      requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!("DiagrammeRsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PNG file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!("rsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PNG file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <-
        add_global_graph_attrs(
          graph, "label", title, "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labelloc", "t", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labeljust", "c", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontname", "Helvetica", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PNG file in the working directory
    rsvg_png(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "PDF" | file_type == "pdf" &&
      requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!("DiagrammeRsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PDF file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')"))
    }

    # Stop function if `rsvg` package is not available
    if (!("rsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PDF file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {
      graph <-
        add_global_graph_attrs(
          graph, "label", title, "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labelloc", "t", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labeljust", "c", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontname", "Helvetica", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PDF file in the working directory
    rsvg_pdf(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if ((file_type == "SVG" | file_type == "svg") &&
      requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!("DiagrammeRsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce an SVG file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!("rsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce an SVG file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <-
        add_global_graph_attrs(
          graph, "label", title, "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labelloc", "t", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labeljust", "c", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontname", "Helvetica", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce an SVG file in the working directory
    rsvg_svg(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "PS" | file_type == "ps" &&
      requireNamespace("DiagrammeRsvg", quietly = TRUE)) {

    # Stop function if `DiagrammeRsvg` package is not available
    if (!("DiagrammeRsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PS file",
          "please install the `DiagrammeRsvg` package and retry",
          "pkg installed using `devtools::install_github('rich-iannone/DiagrammeRsvg')`"))
    }

    # Stop function if `rsvg` package is not available
    if (!("rsvg" %in%
          rownames(utils::installed.packages()))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = c(
          "Cannot currently use this function to produce a PS file",
          "please install the `rsvg` package and retry",
          "pkg installed using `install.packages('rsvg')`"))
    }

    if (!is.null(title)) {

      graph <-
        add_global_graph_attrs(
          graph, "label", title, "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labelloc", "t", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labeljust", "c", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontname", "Helvetica", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontcolor", "gray30", "graph")
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Produce a PS file in the working directory
    rsvg_ps(
      charToRaw(
        DiagrammeRsvg::export_svg(grViz(dot_code))),
      file = file_name,
      width = width,
      height = height)
  }

  if (file_type == "GEXF" | file_type == "gexf") {

    # Convert graph to an `igraph` object
    g <- to_igraph(graph)

    # Determine whether the nodes have a `label`
    # attribute present; if not, use the ID values as
    # the `label` values
    if (is.null(igraph::V(g)$label)) {
      igraph::V(g)$label <- as.character(igraph::V(g))
    }

    # Determine whether the edges have a `weight`
    # attribute present; if not, use a default `weight`
    # value of 1
    if (is.null(igraph::E(g)$weight)) {
      igraph::E(g)$weight <- rep.int(1, igraph::ecount(g))
    }

    # Get a data frame of graph nodes
    nodes <-
      data.frame(cbind(igraph::V(g),
                       igraph::V(g)$label))

    # Get a matrix of graph edges
    edges <-
      ends(graph = g, E(g))

    # Get node attribute names
    node_attr_names <-
      base::setdiff(igraph::vertex_attr_names(g), "label")

    # Get node attributes
    node_attr <-
      data.frame(
        sapply(
          node_attr_names,
          function(attr) {
            sub("&", "&",
                igraph::vertex_attr(g, attr))}))

    # Combine all edge attributes into a matrix
    edge_attr_names <-
      base::setdiff(igraph::edge_attr_names(g), "weight")

    # Get edge attributes
    edge_attr <-
      data.frame(
        sapply(
          edge_attr_names,
          function(attr) { sub("&", "&",
                               igraph::edge_attr(g, attr))}))

    # Combine all graph attributes
    graph_attr <-
      sapply(
        igraph::graph_attr_names(g),
        function(attr) {
          sub("&", "&",
              igraph::graph_attr(g, attr))})

    # Set the file connection
    file_conn <- file(file_name)

    # Create the GEXF object
    output <-
      rgexf::write.gexf(
        nodes, edges,
        edgesWeight = igraph::E(g)$weight,
        edgesAtt = edge_attr,
        nodesAtt = node_attr,
        meta = c(
          list(creator = "none",
               description = "gexf file",
               keywords = "gexf"), graph_attr))

    # Produce a GEXF file in the working directory
    writeLines(as.character(output$graph), file_conn)
  }
}
