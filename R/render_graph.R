#' Render the graph in various formats
#'
#' Using a \code{dgr_graph} object, render the graph in the RStudio Viewer.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param layout a string specifying a layout type to use for node placement in
#'   this rendering. Possible layouts include: \code{nicely}, \code{circle},
#'   \code{tree}, \code{kk}, and \code{fr}.
#' @param output a string specifying the output type; \code{graph} (the default)
#'   renders the graph using the \code{\link{grViz}()} function and
#'   \code{visNetwork} renders the graph using the \code{\link{visnetwork}()}
#'   function.
#' @param title an optional title for a graph when using
#'   \code{output = "graph"}.
#' @param width an optional parameter for specifying the width of the resulting
#'   graphic in pixels.
#' @param height an optional parameter for specifying the height of the
#'   resulting graphic in pixels.
#' @examples
#' \dontrun{
#' # Render a graph that's a
#' # balanced tree
#' create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 3) %>%
#'   render_graph()
#'
#' # Use the `tree` layout for
#' # better node placement in this
#' # hierarchical graph
#' create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 3) %>%
#'   render_graph(layout = "tree")
#'
#' # Plot the same tree graph but
#' # don't show the node ID values
#' create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 3) %>%
#'   set_node_attr_to_display() %>%
#'   render_graph(layout = "tree")
#'
#' # Create a circle graph
#' create_graph() %>%
#'   add_gnm_graph(
#'     n = 55,
#'     m = 75,
#'     set_seed = 23) %>%
#'   render_graph(
#'     layout = "circle")
#'
#' # Render the graph using the
#' # `visNetwork` output option
#' create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 3) %>%
#'   render_graph(
#'     output = "visNetwork")
#' }
#' @importFrom dplyr select rename mutate filter coalesce left_join
#' @importFrom dplyr pull bind_cols as_tibble
#' @importFrom igraph layout_in_circle layout_with_sugiyama
#' @importFrom igraph layout_with_kk layout_with_fr layout_nicely
#' @importFrom purrr flatten_chr
#' @export
render_graph <- function(graph,
                         layout = NULL,
                         output = NULL,
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

  # Create bindings for specific variables
  V1 <- V2 <- x <- y <- attr_type <- value_x <- NULL
  value <- hex <- fillcolor <- new_fillcolor <- NULL

  if (is.null(output)) {
    output <- "graph"
  }

  if (output == "graph") {
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

    # If no fillcolor provided, use default; if no default available,
    # use white
    if (nrow(graph$nodes_df) > 0) {
      if (!("fillcolor" %in% colnames(graph$nodes_df))) {
        if ("fillcolor" %in% graph$global_attrs$attr) {

          graph$nodes_df$fillcolor <-
            graph$global_attrs %>%
            dplyr::filter(attr == "fillcolor" & attr_type == "node") %>%
            dplyr::select(value) %>%
            purrr::flatten_chr()
        } else {
          graph$nodes_df$fillcolor <- "white"
        }
      }
    }

    # If fillcolor is available and there are NA values,
    # replace NAs with default color if available
    if (nrow(graph$nodes_df) > 0) {
      if ("fillcolor" %in% colnames(graph$nodes_df)) {
        if ("fillcolor" %in% graph$global_attrs$attr) {

          graph$nodes_df$fillcolor[which(is.na(graph$nodes_df$fillcolor))] <-
            graph$global_attrs[which(graph$global_attrs$attr == "fillcolor"), 2]
        }
      }
    }

    # Translate X11 colors to hexadecimal colors
    if ("fillcolor" %in% colnames(graph$nodes_df)) {

      graph$nodes_df <-
        graph$nodes_df %>%
        dplyr::left_join(
          x11_hex() %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(hex = toupper(hex)),
          by = c("fillcolor" = "x11_name")) %>%
        dplyr::mutate(new_fillcolor = dplyr::coalesce(hex, fillcolor)) %>%
        dplyr::select(-fillcolor, -hex) %>%
        dplyr::rename(fillcolor = new_fillcolor)
    }

    # Use adaptive font coloring for nodes that have a fill color
    if (!("fontcolor" %in% colnames(graph$nodes_df)) &
        "fillcolor" %in% colnames(graph$nodes_df)) {

      graph$nodes_df$fontcolor <-
        graph$nodes_df$fillcolor %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(value_x = contrasting_text_color(background_color = value)) %>%
        dplyr::pull(value_x)
    }

    if (!is.null(layout)) {
      if (layout %in% c("circle", "tree", "kk", "fr", "nicely")) {

        graph <-
          graph %>%
          add_global_graph_attrs(
            attr = "layout",
            value = "neato",
            attr_type = "graph")

        if ("x" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <-
            graph$nodes_df %>%
            dplyr::select(-x)
        }

        if ("y" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <-
            graph$nodes_df %>%
            dplyr::select(-y)
        }

        if (layout == "circle") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_in_circle() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2) %>%
            dplyr::mutate(x = x * (((count_nodes(graph) + (0.25 * count_nodes(graph)))) / count_nodes(graph))) %>%
            dplyr::mutate(y = y * (((count_nodes(graph) + (0.25 * count_nodes(graph)))) / count_nodes(graph)))
        }

        if (layout == "tree") {
          coords <-
            (graph %>%
               to_igraph() %>%
               igraph::layout_with_sugiyama())[[2]] %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        if (layout == "kk") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_with_kk() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        if (layout == "fr") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_with_fr() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        if (layout == "nicely") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_nicely() %>%
            dplyr::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        # Bind (x, y) coordinates to the graph's
        # internal NDF
        graph$nodes_df <-
          graph$nodes_df %>%
          dplyr::bind_cols(coords)
      }
    }


    if ("image" %in% colnames(graph %>% get_node_df())) {

      # Get a vector of SVG lines
      svg_vec <-
        strsplit(DiagrammeRsvg::export_svg(
          grViz(diagram = graph %>% generate_dot())), "\n") %>%
        unlist()

      # Get a tibble with of SVG data
      svg_tbl <- get_svg_tbl(svg_vec)

      node_id_images <-
        graph %>%
        get_node_df() %>%
        dplyr::select(id, image) %>%
        dplyr::filter(image != "") %>%
        dplyr::pull(id)

      filter_lines <-
        graph %>%
        get_node_df() %>%
        dplyr::select(id, image) %>%
        dplyr::filter(image != "") %>%
        dplyr::mutate(filter_lines = as.character(glue::glue("<filter id=\"{id}\" x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\"><feImage xlink:href=\"{image}\"/></filter>"))) %>%
        dplyr::pull(filter_lines) %>%
        paste(collapse = "\n")

      filter_shape_refs <- as.character(glue::glue(" filter=\"url(#{node_id_images})\" "))

      svg_lines <-
        "<svg display=\"block\" margin=\"0 auto\" position=\"absolute\" width=\"100%\" height=\"100%\""

      svg_line_no <- svg_tbl %>%
        dplyr::filter(type == "svg") %>%
        dplyr::pull(index)

      svg_shape_nos <-
        svg_tbl %>% filter(node_id %in% node_id_images) %>% filter(type == "node_block") %>%
        pull(index)

      svg_shape_nos <- svg_shape_nos + 3
      svg_text_nos <- svg_shape_nos + 1

      # Modify <svg> attrs
      svg_vec[svg_line_no] <- svg_lines

      # Modify shape lines
      for (i in seq(node_id_images)) {

        svg_vec[svg_shape_nos[i]] <-
          sub(" ", paste0(filter_shape_refs[i]), svg_vec[svg_shape_nos[i]])

        svg_vec[svg_text_nos[i]] <- ""
      }

      # Add in <filter> lines
      svg_vec[svg_line_no + 1] <-
        paste0(svg_vec[svg_line_no + 1], "\n\n", filter_lines, "\n")

      svg_vec_1 <- paste(svg_vec, collapse = "\n")

      display <- htmltools::browsable(htmltools::HTML(svg_vec_1))

    } else {

      # Generate DOT code
      dot_code <- generate_dot(graph)

      # Generate a `grViz` object
      grVizObject <-
        grViz(
          diagram = dot_code,
          width = width,
          height = height)

      display <- grVizObject
    }

    display

  } else if (output == "visNetwork") {
    visnetwork(graph)
  }
}
