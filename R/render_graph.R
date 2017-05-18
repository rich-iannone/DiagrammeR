#' Render the graph in various formats
#' @description Using a \code{dgr_graph} object,
#' render the graph in the RStudio Viewer.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param layout a string specifying a layout type to
#' use for node placement in this rendering. Possible
#' layouts include: \code{nicely}, \code{circle},
#' \code{tree}, \code{kk}, and \code{fr}.
#' @param output a string specifying the output type;
#' \code{graph} (the default) renders the graph using
#' the \code{grViz} function and \code{visNetwork}
#' renders the graph using the \code{visnetwork} function.
#' @param title an optional title for a graph when
#' using \code{output = "graph"}.
#' @param width an optional parameter for specifying
#' the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying
#' the height of the resulting graphic in pixels.
#' @examples
#' \dontrun{
#' # Render a graph that's a balanced tree
#' create_graph() %>%
#'   add_balanced_tree(2, 3) %>%
#'   render_graph()
#'
#' # Use the `tree` layout for better node
#' # placement in this hierarchical graph
#' create_graph() %>%
#'   add_balanced_tree(2, 3) %>%
#'   render_graph(layout = "tree")
#'
#' # Plot the same tree graph but don't
#' # show the node ID values
#' create_graph() %>%
#'   add_balanced_tree(2, 3) %>%
#'   set_node_attr_to_display() %>%
#'   render_graph(layout = "tree")
#'
#' # Create a circle graph with
#' create_random_graph(
#'   n = 55, m = 75,
#'   set_seed = 23) %>%
#' render_graph(layout = "circle")
#'
#' # Render the graph using the `visNetwork`
#' # output option
#' create_graph() %>%
#'   add_balanced_tree(2, 3) %>%
#'   render_graph(graph, output = "visNetwork")
#' }
#' @importFrom dplyr select rename mutate filter coalesce left_join
#' @importFrom igraph layout_in_circle layout_with_sugiyama layout_with_kk layout_with_fr layout_nicely
#' @importFrom tibble as_tibble
#' @importFrom purrr map flatten_chr
#' @export render_graph

render_graph <- function(graph,
                         layout = NULL,
                         output = NULL,
                         title = NULL,
                         width = NULL,
                         height = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

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

    # Translate X11 colors to hexadecimal colors
    if ("fillcolor" %in% colnames(graph$nodes_df)) {

      graph$nodes_df <-
        graph$nodes_df %>%
        dplyr::left_join(
          x11_hex() %>%
            tibble::as_tibble() %>%
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
        purrr::map(contrasting_text_color) %>% unlist()
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
            tibble::as_tibble() %>%
            dplyr::rename(x = V1, y = V2) %>%
            dplyr::mutate(x = x * (((node_count(graph) + (0.25 * node_count(graph)))) / node_count(graph))) %>%
            dplyr::mutate(y = y * (((node_count(graph) + (0.25 * node_count(graph)))) / node_count(graph)))
        }

        if (layout == "tree") {
          coords <-
            graph %>%
            to_igraph() %>%
            layout_with_sugiyama() %>%
            .[[2]] %>%
            as_tibble() %>%
            rename(x = V1, y = V2)
        }

        if (layout == "kk") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_with_kk() %>%
            tibble::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        if (layout == "fr") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_with_fr() %>%
            tibble::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        if (layout == "nicely") {
          coords <-
            graph %>%
            to_igraph() %>%
            igraph::layout_nicely() %>%
            tibble::as_tibble() %>%
            dplyr::rename(x = V1, y = V2)
        }

        # Bind (x, y) coordinates to the graph's
        # internal NDF
        graph$nodes_df <-
          graph$nodes_df %>%
          bind_cols(coords)
      }
    }

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Generate a `grViz` object
    grVizObject <-
      grViz(
        diagram = dot_code,
        width = width,
        height = height)

    grVizObject

  } else if (output == "visNetwork") {
    visnetwork(graph)
  }
}
